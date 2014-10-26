{-# LANGUAGE OverloadedStrings #-}
-- | "Middle end" for MCCtl. This is the part that listens for DBus requests
--   handles operations involving instance files. Calls on Backend to talk to
--   the Minecraft server process.
module MCCtl.MiddleEnd (startMCCtlServer) where
import qualified Data.Map.Strict as M
import Data.Int
import System.Posix.Process
import System.Directory
import Control.Applicative
import Control.Monad
import Control.Concurrent
import DBus.Client
import MCCtl.Config
import MCCtl.Config.Parser
import MCCtl.Paths
import qualified MCCtl.Backend as Backend

type Instances = MVar (M.Map String Backend.State)

-- | Start the MCCtl server, as well as any autostart instances.
startMCCtlServer :: GlobalConfig -> IO ()
startMCCtlServer cfg = do
  case cfgConfigPath cfg of
    Directory d -> do
      isdir <- doesDirectoryExist d
      if isdir
        then forkServer
        else putStrLn $ "instance directory '" ++ d ++ "' does not exist"
    File f -> do
      isok <- checkInstanceFile f
      if isok
        then forkServer
        else putStrLn $ "file '" ++ f ++ "' is not a valid instance file"
  where
    shutdown done insts client = do
      void $ stop insts ""
      void $ releaseName client dbusBus
      putMVar done ()

    forkServer = void . forkProcess $ do
      c <- connectSystem
      namerep <- requestName c dbusBus [nameDoNotQueue]
      case namerep of
        NamePrimaryOwner -> do
          insts <- newMVar M.empty
          closeLock <- newEmptyMVar
          export c dbusObj [
              autoMethod dbusIface "shutdown"   $ shutdown closeLock insts c,
              autoMethod dbusIface "start"      $ start False cfg insts,
              autoMethod dbusIface "stop"       $ stop insts,
              autoMethod dbusIface "command"    $ command insts,
              autoMethod dbusIface "backlog"    $ backlog insts,
              autoMethod dbusIface "configpath" $ getConfigPath cfg,
              autoMethod dbusIface "create"     $ create cfg,
              autoMethod dbusIface "delete"     $ delete cfg insts
            ]
          void $ start True cfg insts ""
          takeMVar $ closeLock
          disconnect c
        _ -> do
          return ()


-- | Create a new instance by the given name, unless we're in single instance
--   mode or if the named instance already exists.
create :: GlobalConfig -> String -> FilePath -> IO String
create cfg@(GlobalConfig {cfgConfigPath = Directory _}) name srvdir = do
  fileexists <- doesFileExist file
  direxists <- doesDirectoryExist srvdir
  case (fileexists, direxists) of
    (True, _)      -> return "instance already exists"
    (_, True)      -> return "server directory already exists"
    (False, False) -> writeFile file (defaultConfig srvdir) >> return ""
  where
    file = instanceFilePath cfg name
create _ _ _ = do
  return "can't create instances in single instance mode"

-- | Stop and delete an instance.
delete :: GlobalConfig -> Instances -> String -> Bool -> IO String
delete cfg insts name deletedatadir = do
    minst <- readInstanceFile file
    case minst of
      Just inst -> do
        let srvdir = serverDirectory inst
        void $ stop insts name
        direxists <- doesDirectoryExist srvdir
        when (deletedatadir && direxists) $ do
          removeDirectoryRecursive srvdir
        removeFile $ instanceFilePath cfg name
        return ""
      _ -> do
        return "no such instance"
  where
    file = instanceFilePath cfg name

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
        exists <- doesFileExist instfile
        if exists
          then do
            minst <- readInstanceFile instfile
            case minst of
              Just inst
                | not only_auto || autostart inst -> do
                  est <- Backend.start insts (Config name cfg inst)
                  case est of
                    Right st -> return (M.insert name st m, "")
                    Left err -> return (m, err)
                | otherwise -> do
                  return (m, "") -- Only autostart requested, and we don't
              _ -> do
                return (m, "unable to parse instance file '" ++instfile++ "'")
          else do
            return (m, "no such instance")
  where
    instfile = instanceFilePath cfg name

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
command :: Instances -> String -> String -> IO String
command insts "" cmd = withMVar insts $ \m -> do
  reps <- mapM (Backend.command cmd . snd) $ M.toList m
  return $ unlines reps
command insts name cmd = withMVar insts $ \m -> do
  case M.lookup name m of
    Just st -> Backend.command cmd st
    _       -> return "no such instance running"

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
