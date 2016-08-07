-- | MCCtl backend; this is the part that actually talks to the Minecraft
--   server.
module MCCtl.Backend (
    State, start, stop, backlog, command, backup
  ) where
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import Data.ByteString.UTF8
import Data.Time
import Data.Int
import System.IO
import System.Process
import System.FilePath
import System.Directory
import System.Exit
import Control.Monad
import Control.Concurrent
import MCCtl.Config
import MCCtl.Paths

-- | State for a single instance.
data State = State {
    -- | Minecraft server Java process handle; used to wait for server
    --   shutdown.
    stProcHdl   :: !ProcessHandle,

    -- | Server process' stdin handle. Used to send commands to the server.
    stInHdl     :: !Handle,

    -- | Server process' stdout handle. Used to extract result messages after
    --   sending a command.
    stOutHdl    :: !(MVar Handle),

    -- | Per instance and global config for instance.
    stConfig    :: !Config,

    -- | Full when the instance is done and has exited cleanly.
    stDoneLock  :: !(MVar ()),

    -- | When was the instance last (re)started?
    stStartTime :: !UTCTime
  }

-- | Start a new server process, complete with log eater and all.
start :: MVar (M.Map String State) -> Config -> IO (Either String State)
start insts cfg = do
    putStrLn $ "Starting instance '" ++ name ++ "'..."
    est <- spawnServerProc cfg
    case est of
      Right st -> do
        logeater <- forkIO $ discardLogLines st
        void . forkIO $ monitorServerProc st logeater
      Left err -> do
        putStrLn err
    return est
  where
    name = instanceName cfg
    monitorServerProc st logeater = do
      ec <- waitForProcess $ stProcHdl st
      killThread logeater
      case ec of
        ExitSuccess -> do
          putStrLn $ "Instance '" ++ name ++ "' exited cleanly"
          putMVar (stDoneLock st) ()
        ExitFailure _ -> do
          modifyMVar_ insts $ pure . M.delete name
          t <- getCurrentTime
          case restartOnFailure $ instanceConfig cfg of
            Restart cooldown
              | diffUTCTime t (stStartTime st) > cooldown -> do
                putStrLn $ "Instance '" ++ name ++
                           "' crashed unexpectedly; restarting!"
                modifyMVar_ insts $ \m -> do
                  newst <- start insts cfg
                  case newst of
                    Right newst' -> do
                      return $ M.insert name newst' m
                    Left err -> do
                      putStrLn $ "Unable to restart instance'" ++
                                 name ++ "': " ++ err
                      return m
              | otherwise -> do
                putStrLn $ "Instance '" ++ name ++ "' crashed " ++
                           "unexpectedly, but is crashing too fast to be " ++
                           "restarted!"
            DontRestart -> do
                putStrLn $ "Instance '" ++ name ++ "' crashed " ++
                           "unexpectedly, but is not configured to restart!"

-- | Send a stop message to the server, then wait for it to exit.
stop :: State -> IO ()
stop st = do
  putStrLn $ "Stopping instance '" ++ (instanceName $ stConfig st)
  writeCommand st "stop"
  takeMVar $ stDoneLock st

-- | Perform a command, wait for 100ms, then examine the log for a result.
command :: String -> State -> IO String
command cmd st = do
  withMVar (stOutHdl st) $ \h -> do
    discardLines h
    writeCommand st cmd
    threadDelay 100000
    ls <- readLines h
    return $ unlines $ map toString ls

-- | Get the n latest entries in the log file.
backlog :: Int32 -> State -> IO String
backlog n st = do
    readProcess "/usr/bin/tail" ["-n", show n, logfile] ""
  where
    logfile =
      serverDirectory (instanceConfig $ stConfig st) </> "logs" </> "latest.log"

-- | Back up an instance to its backup directory.
backup :: State -> IO String
backup st = do
    now <- showTime <$> getCurrentTime
    case backupDirectory $ instanceConfig $ stConfig st of
      Just backupdir -> do
        let file = backupdir </> now <.> "tar.bz2"
            parentdir = datadir </> ".."
            dir = takeBaseName datadir
            taropts = ["-C", parentdir, "-cjf", file, dir]

        writeCommand st "save-off"
        writeCommandSync st "save-all" $ \ln ->
          dropWhile (/=']') ln == "] [Server thread/INFO]: Saved the world"

        -- MC reports "saved the world" slightly before it's done saving
        -- for some bizarre reason, so we have to wait a little longer.
        threadDelay 1000000
        createDirectoryIfMissing True backupdir
        void $ readProcess "/bin/tar" taropts ""

        writeCommand st "save-on"
        return $ "instance '" ++ name ++ "' backed up to '" ++ file ++ "'"
      Nothing -> do
        return $ "no backup directory configured for instance '"++name++"'"
  where
    showTime = formatTime defaultTimeLocale timespec
    timespec = concat [name, "_%Y-%m-%d_%H:%M:%S"]
    datadir = serverDirectory $ instanceConfig $ stConfig st
    name = instanceName $ stConfig st

-- | Write a command to the instance server.
writeCommand :: State -> String -> IO ()
writeCommand st cmd = hPutStrLn (stInHdl st) cmd >> hFlush (stInHdl st)

-- | Write a command to the instance server, then wait until a line satisfying
--   the given predicate appears in the log before returning.
--   Beware; the instance will not be able to accept any other commands until
--   this function returns.
writeCommandSync :: State -> String -> (String -> Bool) -> IO ()
writeCommandSync st cmd awaited = withMVar (stOutHdl st) $ \h -> do
    discardLines h
    writeCommand st cmd
    waitForPredOn h
  where
    waitForPredOn h = do
      ln <- hGetLine h
      unless (awaited ln) $ waitForPredOn h

-- | Spawn a Minecraft server process.
spawnServerProc :: Config -> IO (Either String State)
spawnServerProc cfg = do
  mjava <- findExecutable java
  case mjava of
    Just _ -> do
      createDirectoryIfMissing True dir
      writeFile (dir </> "eula.txt") "eula=true"
      case serverProperties $ instanceConfig cfg of
        Just props -> writeFile (dir </> "server.properties") props
        _          -> return ()
      (Just i, Just o, Nothing, ph) <- createProcess cp
      Right <$> (State <$> pure ph
                       <*> pure i
                       <*> newMVar o
                       <*> pure cfg
                       <*> newEmptyMVar
                       <*> getCurrentTime)
    _ -> do
      return $ Left "Java binary not found at /usr/bin/java"
  where
    java = "/usr/bin/java"
    jar = instanceJAR cfg
    dir = instanceDirectory cfg
    xms = "-Xms" ++ show (initialHeapSize $ instanceConfig cfg) ++ "M"
    xmx = "-Xmx" ++ show (maxHeapSize $ instanceConfig cfg) ++ "M"
    concgc = "-XX:+UseConcMarkSweepGC"
    inccp = "-XX:+CMSIncrementalPacing"
    aggropts = "-XX:+AggressiveOpts"
    opts = [concgc, inccp, aggropts, xms, xmx, "-jar", jar, "nogui"]
    cp = CreateProcess {
        cmdspec       = RawCommand java opts,
        cwd           = Just dir,
        env           = Nothing,
        std_in        = CreatePipe,
        std_out       = CreatePipe,
        std_err       = Inherit,
        close_fds     = False,
        create_group  = False,
        delegate_ctlc = False
      }

-- | Wait 10 seconds, then discard any lines from Minecraft's stdout.
--   Repeat indefinitely.
discardLogLines :: State -> IO ()
discardLogLines st = forever $ do
  threadDelay 10000000
  withMVar (stOutHdl st) discardLines

-- | Discard any lines currently waiting to be read from the given handle.
discardLines :: Handle -> IO ()
discardLines h = do
  rdy <- hReady h
  case rdy of
    True -> BS.hGetLine h >> discardLines h
    _    -> return ()

-- | Read any lines waiting to be read from the given handle.
readLines :: Handle -> IO [BS.ByteString]
readLines h = do
  rdy <- hReady h
  case rdy of
    True -> do
      l <- BS.hGetLine h
      ls <- readLines h
      return (l:ls)
    _    -> do
      return []
