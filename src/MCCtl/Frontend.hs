{-# LANGUAGE OverloadedStrings #-}
module MCCtl.Frontend where
import Data.Int (Int32)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Control.Monad
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import DBus
import MCCtl.DBus
import MCCtl.Config
import MCCtl.Config.Parser

-- | Current status of an instance. It may either be running, exist but not
--   run, or just not exist.
data InstanceStatus = Running | NotRunning | NotFound
  deriving (Read, Show, Eq, Ord)

-- | Run a command on the mcctl server, then print the results.
runAndPrint :: MemberName -> [Variant] -> IO ()
runAndPrint cmd args = do
  ret <- dbusCall cmd args
  case fromVariant <$> methodReturnBody ret of
    [Just s] -> printMessage s
    _        -> error "Impossibru!"

-- | List all available instances.
listInstances :: IO ()
listInstances = runAndPrint "list" []

-- | Get the identifiers of all running instances.
getRunningInstances :: IO [String]
getRunningInstances = do
  ret <- dbusCall "list" []
  case fromVariant <$> methodReturnBody ret of
    [Just s] -> return [ reverse $ drop 10 $ reverse ln
                       | ln <- lines s, "[running]" `isSuffixOf` ln]
    _        -> error "Impossibru!"

-- | Print the running/not running status of the given instance.
instanceStatus :: String -> IO ()
instanceStatus inst = do
  st <- getInstanceStatus inst
  case st of
    Running    -> putStrLn "running"
    NotRunning -> putStrLn "not running"
    NotFound   -> putStrLn "no such instance"

-- | Get the status of the given instance.
getInstanceStatus :: String -> IO InstanceStatus
getInstanceStatus inst = do
  ret <- dbusCall "list" []
  case fromVariant <$> methodReturnBody ret of
    [Just s]
      | inst `isPrefixOf` s -> return $ running s
      | otherwise           -> return NotFound
    _                       -> error "Impossibru!"
  where
    running :: String -> InstanceStatus
    running s
      | "[running]" `isSuffixOf` (head $ lines s) = Running
      | otherwise                                 = NotRunning

-- | Back up a running instance.
backupInstance :: String -> IO ()
backupInstance name = runAndPrint "backup" [toVariant name]

-- | Delete an instance and (optionally) all of its associated data.
deleteInstance :: GlobalConfig -> String -> IO ()
deleteInstance cfg name = do
  case cfgForce cfg of
    True -> runAndPrint "delete" [toVariant name, toVariant deletedata]
    _    -> putStrLn dangerous
  where
    deletedata = cfgDeleteDataDir cfg
    dangerous = concat [
      "WARNING: your are about to PERMANENTLY delete the instance '", name,
      "'", andData, "!\n",
      "If you are really sure about this, run 'mcctl delete ", name,
      " --force'."]
    andData | deletedata = " and all of its server data"
            | otherwise  = ""

-- | Create a new instance by the given name. Prints an error message if mcctl
--   is configured to run in single instance mode.
createInstance :: String -> FilePath -> IO ()
createInstance name srvdir = runAndPrint "create" $ map toVariant [name, dir]
  where dir = if null srvdir then name else srvdir

-- | Ask the server for the path to the instance config for the given instance,
--   then let the user modified it with their chosen $EDITOR. Check that the
--   config is actually a valid config before atomically overwriting the old
--   one.
editConfigWith :: Client -> GlobalConfig -> String -> IO ()
editConfigWith client cfg name = do
    ret <- dbusCallWith client "configpath" [toVariant name]
    case fromVariant <$> methodReturnBody ret of
      [Just ["ok", file]] -> edit file
      [Just [err]]        -> putStrLn err
      _                   -> error "Impossibru!"
  where
    edit file = do
      bin <- maybe "nano" id <$> lookupEnv "EDITOR"
      meditor <- findExecutable bin
      case meditor of
        Just editor -> do
          let editfile = "/tmp" </> takeFileName file <.> "edit"
          exists <- doesFileExist editfile
          when (not exists || not (cfgResumeEdit cfg)) $ do
            copyFile file editfile
          void $ spawnProcess editor [editfile] >>= waitForProcess
          isok <- checkInstanceFile editfile
          if isok
            then do
              void $ dbusCallWith client "commitcfg" [toVariant name]
              removeFile editfile
            else do
              putStrLn badconfig
        _ -> do
          putStrLn noeditor
    noeditor = "Unable to find a suitable editor.\n" ++
               "Either install nano or set your $EDITOR environment variable."
    badconfig = "Your config does not validate, and so has not been " ++
                "updated.\n" ++
                "You can continue editing this configuration by running " ++
                "'mcctl edit --resume',\n" ++
                "or start over with your known good config by running " ++
                "'mcctl edit'."

-- | Like 'editConfigWith', but opens a new DBus session.
editConfig :: GlobalConfig -> String -> IO ()
editConfig cfg name = do
  client <- connectSystem
  editConfigWith client cfg name
  disconnect client

-- | Perform a gracious shutdown of all instances, then kill the MCCtl server.
shutdownServer :: IO ()
shutdownServer = dbusCall "shutdown" [] >> return ()

-- | Tell an instance to shut down, then wait for it to actually do so.
stopServer :: String -> IO ()
stopServer name = runAndPrint "stop" [toVariant name]

-- | Start an instance unless it's already running.
startServer :: String -> IO ()
startServer name = runAndPrint "start" [toVariant name]

-- | Execute a command on the given server, then print the response.
serverCommand :: String -> String -> IO ()
serverCommand name cmd = runAndPrint "command" [toVariant name, toVariant cmd]

-- | Import a world directory as an MCCtl instance.
importWorld :: GlobalConfig -> String -> FilePath -> IO ()
importWorld cfg name dir = do
  isdir <- doesDirectoryExist dir
  st <- getInstanceStatus name
  case (isdir, st) of
    (True, NotFound) -> do
      dir' <- makeAbsolute dir
      client <- connectSystem
      res <- dbusCallWith client "import" [toVariant name, toVariant dir']
      let [Just res'] = fromVariant <$> methodReturnBody res
      when (null res') $ editConfigWith client cfg name
      printMessage res'
    (False, _) -> do
      putStrLn $ "directory `" ++ dir ++ "' does not exist"
    _ -> do
      putStrLn "instance already exists"

-- | Print the last n lines from the server log.
getServerBacklog :: String -> Int32 -> IO ()
getServerBacklog name n = runAndPrint "backlog" [toVariant name, toVariant n]

-- | Print a message followed by a newline. If the message is empty, print
--   nothing.
printMessage :: String -> IO ()
printMessage s =
  case strip s of
    "" -> return ()
    s' -> putStrLn s'
  where
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
