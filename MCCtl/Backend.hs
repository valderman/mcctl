{-# LANGUAGE OverloadedStrings #-}
-- | MCCtl backend; this is the part that actually talks to the Minecraft
--   server.
module MCCtl.Backend (startServer) where
import qualified Data.ByteString as BS
import Data.Int
import System.IO
import System.Process
import System.Posix.Process
import System.FilePath
import Control.Applicative
import Control.Monad
import Control.Concurrent
import DBus.Client
import MCCtl.Config

data Instance = Instance {
    stProcHdl   :: !ProcessHandle,
    stInHdl     :: !Handle,
    stOutHdl    :: !(MVar Handle),
    stCloseLock :: !(MVar ()),
    stConfig    :: !Config
  }

startServer :: Config -> IO ()
startServer cfg = void . forkProcess $ do
  client <- connectSystem
  namerep <- requestName client dbusBus [nameDoNotQueue]
  case namerep of
    NamePrimaryOwner -> do
      st <- spawnServerProc cfg
      export client dbusObj [
          autoMethod dbusIface "stop"    $ stop st,
          autoMethod dbusIface "command" $ command st,
          autoMethod dbusIface "backlog" $ backlog st
        ]
      void . forkIO $ discardLogLines st
      takeMVar $ stCloseLock st
      void $ releaseName client "cc.ekblad.mcctl"
      disconnect client
    _ -> do
      return ()

-- | Send a stop message to the server, then wait for it to exit.
stop :: Instance -> IO ()
stop st = do
  hPutStrLn (stInHdl st) "stop"
  hFlush $ stInHdl st
  void . waitForProcess $ stProcHdl st
  putMVar (stCloseLock st) ()

-- | Perform a command, wait for 100ms, then examine the log for a result.
command :: Instance -> String -> IO [BS.ByteString]
command st cmd = do
  putStrLn "command taking lock"
  withMVar (stOutHdl st) $ \h -> do
    discardLines h
    hPutStrLn (stInHdl st) cmd
    hFlush $ stInHdl st
    threadDelay 100000
    ls <- readLines h
    return ls

-- | Get the n latest entries in the log file.
backlog :: Instance -> Int32 -> IO String
backlog st n = do
    readProcess "tail" ["-n", show n, logfile] ""
  where
    logfile = serverDirectory (stConfig st) </> "logs" </> "latest.log"

-- | Spawn a Minecraft server process.
spawnServerProc :: Config -> IO Instance
spawnServerProc cfg@(Config {serverJAR = jar, serverDirectory = dir}) = do
    (Just i, Just o, Nothing, ph) <- createProcess cp
    Instance <$> pure ph
             <*> pure i
             <*> newMVar o
             <*> newEmptyMVar
             <*> pure cfg
  where
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
discardLogLines :: Instance -> IO ()
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
