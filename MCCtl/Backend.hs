{-# LANGUAGE OverloadedStrings #-}
-- | MCCtl backend; this is the part that actually talks to the Minecraft
--   server.
module MCCtl.Backend (startMCCtlServer) where
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Int
import System.IO
import System.Process
import System.Posix.Process
import System.FilePath
import System.Directory
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Exception
import DBus.Client
import MCCtl.Config
import MCCtl.Config.Parser
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
    stConfig    :: !Config
  }

-- | Start the MCCtl server, as well as any autostart instances.
startMCCtlServer :: GlobalConfig -> IO ()
startMCCtlServer cfg = void . forkProcess $ do
  client <- connectSystem
  namerep <- requestName client dbusBus [nameDoNotQueue]
  case namerep of
    NamePrimaryOwner -> do
      insts <- newMVar M.empty
      closeLock <- newEmptyMVar
      export client dbusObj [
          autoMethod dbusIface "shutdown" $ putMVar closeLock (),
          autoMethod dbusIface "start"    $ start False cfg insts,
          autoMethod dbusIface "stop"     $ stop insts,
          autoMethod dbusIface "command"  $ command insts,
          autoMethod dbusIface "backlog"  $ backlog insts
        ]
      void $ start True cfg insts ""
      takeMVar $ closeLock
      void $ stop insts ""
      void $ releaseName client dbusBus
      disconnect client
    _ -> do
      return ()

-- | Start a new instance, or all eligible instances if name is the empty
--   string.
start :: Bool                      -- ^ Only start autostart instances.
      -> GlobalConfig              -- ^ Global config.
      -> MVar (M.Map String State) -- ^ All currently running instances.
      -> String                    -- ^ Name of instance to start.
      -> IO String                 -- ^ Any output resulting from the attempted
                                   --   start.
start only_auto cfg insts "" = do
  is <- getAllInstances cfg
  unlines <$> mapM (start only_auto cfg insts) is
start only_auto cfg insts name = modifyMVar insts $ \m -> do
    case M.lookup name m of
      Just _ -> do
        return (m, "instance '" ++ name ++ "' is already running")
      _ -> do
        i <- try (BS.readFile instf) :: IO (Either SomeException BS.ByteString)
        case i of
          Right i' -> do
            case parseInstance i' of
              Just inst -> do
                if (not only_auto || autostart inst)
                  then do
                    st <- start' $ Config name cfg inst
                    return (M.insert name st m, "")
                  else do
                    return (m, "")
              _ -> do
                return (m, "unable to parse instance file '" ++ instf ++ "'")
          _ -> do
            return (m, "file does not exist: '" ++ instf ++ "'")
  where
    instf = instanceFilePath cfg name

-- | Stop a running instance, or all running instances if name is the empty
--   string.
stop :: MVar (M.Map String State) -> String -> IO String
stop insts "" = modifyMVar insts $ \m -> do
  mapM_ (stop' . snd) $ M.toList m
  return (M.empty, "")
stop insts name = modifyMVar insts $ \m -> do
  case M.lookup name m of
    Just st -> do
      stop' st
      return (M.delete name m, "")
    _ -> do
      return (m, "no such instance running")

-- | Sent a command to a instance, or all if name is the empty string.
command :: MVar (M.Map String State) -> String -> String -> IO [BS.ByteString]
command insts "" cmd = withMVar insts $ \m -> do
  reps <- mapM (command' cmd . snd) $ M.toList m
  return $ concat reps
command insts name cmd = withMVar insts $ \m -> do
  case M.lookup name m of
    Just st -> command' cmd st
    _       -> return ["no such instance running"]

-- | Get the last n lines from the log of the given instance, or all instances
--   if name is the empty string.
backlog :: MVar (M.Map String State) -> String -> Int32 -> IO String
backlog insts "" n = withMVar insts $ \m -> do
  reps <- mapM (backlog' n . snd) $ M.toList m
  return $ unlines reps
backlog insts name n = withMVar insts $ \m -> do
  case M.lookup name m of
    Just st -> backlog' n st
    _       -> return "no such instance running"

-- | Start a new server process, complete with log eater and all.
start' :: Config -> IO State
start' cfg = do
  st <- spawnServerProc cfg
  void . forkIO $ discardLogLines st
  return st

-- | Send a stop message to the server, then wait for it to exit.
stop' :: State -> IO ()
stop' st = do
  hPutStrLn (stInHdl st) "stop"
  hFlush $ stInHdl st
  void . waitForProcess $ stProcHdl st

-- | Perform a command, wait for 100ms, then examine the log for a result.
command' :: String -> State -> IO [BS.ByteString]
command' cmd st = do
  withMVar (stOutHdl st) $ \h -> do
    discardLines h
    hPutStrLn (stInHdl st) cmd
    hFlush $ stInHdl st
    threadDelay 100000
    ls <- readLines h
    return ls

-- | Get the n latest entries in the log file.
backlog' :: Int32 -> State -> IO String
backlog' n st = do
    readProcess "/usr/bin/tail" ["-n", show n, logfile] ""
  where
    logfile =
      serverDirectory (instanceConfig $ stConfig st) </> "logs" </> "latest.log"

-- | Spawn a Minecraft server process.
spawnServerProc :: Config -> IO State
spawnServerProc cfg = do
    createDirectoryIfMissing True dir
    writeFile (dir </> "eula.txt") "eula=true"
    case serverProperties $ instanceConfig cfg of
      Just props -> writeFile (dir </> "server.properties") props
      _          -> return ()
    (Just i, Just o, Nothing, ph) <- createProcess cp
    State <$> pure ph
          <*> pure i
          <*> newMVar o
          <*> pure cfg
  where
    jar = instanceJAR cfg
    dir = instanceDirectory cfg
    cp = CreateProcess {
        cmdspec       = RawCommand "/usr/bin/java" ["-jar", jar, "nogui"],
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
discardLogLines st = do
  threadDelay 10000000
  eof <- withMVar (stOutHdl st) $ \h -> do
    eof <- hIsClosed h
    if eof then return True
           else discardLines h >> return False
  unless eof $ discardLogLines st

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
