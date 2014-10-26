{-# LANGUAGE OverloadedStrings #-}
module MCCtl.Frontend where
import Data.Int (Int32)
import Data.Char (isSpace)
import Control.Applicative
import Control.Monad
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import DBus
import MCCtl.DBus
import MCCtl.Config
import MCCtl.Config.Parser

-- | Run a command on the mcctl server, then print the results.
runAndPrint :: MemberName -> [Variant] -> IO ()
runAndPrint cmd args = do
  ret <- dbusCall cmd args
  case fromVariant <$> methodReturnBody ret of
    [Just s] -> printMessage s
    _        -> error "Impossibru!"

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
editConfig :: GlobalConfig -> String -> IO ()
editConfig cfg name = do
    ret <- dbusCall "configpath" [toVariant name]
    case fromVariant <$> methodReturnBody ret of
      [Just ["ok", file]] -> edit file
      [Just [err]]        -> putStrLn err
      _                   -> error "Impossibru!"
  where
    edit file = do
      bin <- maybe "/usr/bin/nano" id <$> lookupEnv "EDITOR"
      meditor <- findExecutable bin
      case meditor of
        Just editor -> do
          let editfile = file <.> "edit"
          exists <- doesFileExist editfile
          when (not exists || not (cfgResumeEdit cfg)) $ do
            copyFile file (file <.> "edit")
          void $ spawnProcess editor [editfile] >>= waitForProcess
          isok <- checkInstanceFile editfile
          if isok
            then renameFile editfile file
            else putStrLn badconfig
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

-- | Print the last n lines from the server log.
getServerBacklog :: String -> Int32 -> IO ()
getServerBacklog name n = runAndPrint "backlog" [toVariant name, toVariant n]

printMessage :: String -> IO ()
printMessage "" = return ()
printMessage s  = putStrLn $ strip s
  where strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
