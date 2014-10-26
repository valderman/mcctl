{-# LANGUAGE OverloadedStrings #-}
module MCCtl.Frontend where
import qualified Data.ByteString.Char8 as BS (putStrLn)
import Data.Int
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
      x                   -> error $ show x -- error "Impossibru!"
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
stopServer name = do
  ret <- dbusCall "stop" [toVariant name]
  case fromVariant <$> methodReturnBody ret of
    [Just s] -> printMessage s
    _        -> error "Impossibru!"

-- | Start an instance unless it's already running.
startServer :: String -> IO ()
startServer name = do
  ret <- dbusCall "start" [toVariant name]
  case fromVariant <$> methodReturnBody ret of
    [Just s] -> printMessage s
    _        -> error "Impossibru!"

-- | Execute a command on the given server, then print the response.
serverCommand :: String -> String -> IO ()
serverCommand name cmd = do
  ret <- dbusCall "command" [toVariant name, toVariant cmd]
  case fromVariant <$> methodReturnBody ret of
    [Just bs] -> mapM_ BS.putStrLn $ reverse bs
    _         -> error "Impossibru!"

-- | Print the last n lines from the server log.
getServerBacklog :: String -> Int32 -> IO ()
getServerBacklog name n = do
  ret <- dbusCall "backlog" [toVariant name, toVariant n]
  case fromVariant <$> methodReturnBody ret of
    [Just s] -> printMessage s
    _        -> error "Impossibru!"

printMessage :: String -> IO ()
printMessage "" = return ()
printMessage s  = putStrLn s
