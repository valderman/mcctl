{-# LANGUAGE OverloadedStrings #-}
module MCCtl.Frontend where
import qualified Data.ByteString.Char8 as BS (putStrLn)
import Data.Int
import Control.Applicative
import DBus
import MCCtl.DBus

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
