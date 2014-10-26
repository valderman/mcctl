{-# LANGUAGE OverloadedStrings #-}
-- | "Middle end" for MCCtl. This is the part that listens for DBus requests
--   and calls out to the backend to actually talk to the server.
module MCCtl.MiddleEnd (startMCCtlServer) where
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Int
import System.Posix.Process
import System.Directory (doesFileExist)
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Exception
import DBus.Client
import MCCtl.Config
import MCCtl.Config.Parser
import MCCtl.Paths
import qualified MCCtl.Backend as Backend

type Instances = MVar (M.Map String Backend.State)

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
            autoMethod dbusIface "shutdown"   $ shutdown closeLock insts client,
            autoMethod dbusIface "start"      $ start False cfg insts,
            autoMethod dbusIface "stop"       $ stop insts,
            autoMethod dbusIface "command"    $ command insts,
            autoMethod dbusIface "backlog"    $ backlog insts,
            autoMethod dbusIface "configpath" $ getConfigPath cfg
          ]
        void $ start True cfg insts ""
        takeMVar $ closeLock
        disconnect client
      _ -> do
        return ()
  where
    shutdown done insts client = do
      void $ stop insts ""
      void $ releaseName client dbusBus
      putMVar done ()

-- | Get the instance configuration path for the given instance.
--   Returns ["ok", path] if a config could be unambiguously chosen, or
--   [error message] if it couldn't.
getConfigPath :: GlobalConfig -> String -> IO [String]
getConfigPath cfg "" = do
  insts <- getAllInstances cfg
  case insts of
    [inst] -> return ["ok", instanceFilePath cfg inst]
    []     -> return ["no available instances"]
    is     -> return ["command ambiguous; choose an instance: " ++ show is]
getConfigPath cfg name = do
    isfile <- doesFileExist file
    if isfile
      then return ["ok", file]
      else return ["instance '" ++ name ++ "' does not exist"]
  where
    file = instanceFilePath cfg name

-- | Start a new instance, or all eligible instances if name is the empty
--   string.
start :: Bool         -- ^ Only start autostart instances.
      -> GlobalConfig -- ^ Global config.
      -> Instances    -- ^ All currently running instances.
      -> String       -- ^ Name of instance to start.
      -> IO String    -- ^ Any output resulting from the attempted start.
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
                    st <- Backend.start insts (Config name cfg inst)
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
stop :: Instances -> String -> IO String
stop insts "" = modifyMVar insts $ \m -> do
  mapM_ (Backend.stop . snd) $ M.toList m
  return (M.empty, "")
stop insts name = modifyMVar insts $ \m -> do
  case M.lookup name m of
    Just st -> do
      Backend.stop st
      return (M.delete name m, "")
    _ -> do
      return (m, "no such instance running")

-- | Sent a command to a instance, or all if name is the empty string.
command :: Instances -> String -> String -> IO [BS.ByteString]
command insts "" cmd = withMVar insts $ \m -> do
  reps <- mapM (Backend.command cmd . snd) $ M.toList m
  return $ concat reps
command insts name cmd = withMVar insts $ \m -> do
  case M.lookup name m of
    Just st -> Backend.command cmd st
    _       -> return ["no such instance running"]

-- | Get the last n lines from the log of the given instance, or all instances
--   if name is the empty string.
backlog :: Instances -> String -> Int32 -> IO String
backlog insts "" n = withMVar insts $ \m -> do
  reps <- mapM (Backend.backlog n . snd) $ M.toList m
  return $ unlines reps
backlog insts name n = withMVar insts $ \m -> do
  case M.lookup name m of
    Just st -> Backend.backlog n st
    _       -> return "no such instance running"
