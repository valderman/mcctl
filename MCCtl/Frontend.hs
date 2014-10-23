{-# LANGUAGE OverloadedStrings #-}
module MCCtl.Frontend where
import qualified Data.ByteString.Char8 as BS (putStrLn)
import Data.Int
import Control.Applicative
import DBus
import MCCtl.DBus

-- | Tell a server to shut down, then wait for it to actually do so.
stopServer :: IO ()
stopServer = dbusCall "stop" [] >> return ()

-- | Execute a command on the server, then print the response.
serverCommand :: String -> IO ()
serverCommand cmd = do
  ret <- dbusCall "command" [toVariant cmd]
  case fromVariant <$> methodReturnBody ret of
    [Just bs] -> mapM_ BS.putStrLn $ reverse bs
    _         -> error "Impossibru!"

-- | Print the last n lines from the server log.
getServerBacklog :: Int32 -> IO ()
getServerBacklog n = do
  ret <- dbusCall "backlog" [toVariant n]
  case fromVariant <$> methodReturnBody ret of
    [Just bs] -> mapM_ BS.putStrLn $ reverse bs
    _         -> error "Impossibru!"
