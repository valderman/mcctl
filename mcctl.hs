{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.Console.GetOpt
import Control.Exception
import Data.Maybe
import Data.Default
import MCCtl.Frontend
import MCCtl.Backend
import MCCtl.Config

main :: IO ()
main = do
  args <- getArgs
  let (opts, cmds, _) = getOpt Permute options args
      opts' = listToMaybe $ reverse $ catMaybes opts
      cfgfile = maybe "/etc/mcctl.cfg" id opts'
  cfgstr <- try (readFile "/etc/mcctl.cfg") :: IO (Either SomeException String)
  case fmap reads cfgstr of
    Right [(cfg, "")] -> do
      case cmds of
        []             -> putStrLn "Usage: mcctl start|stop|restart|command"
        ["start"]      -> startServer cfg
        ["stop"]       -> stopServer
        ["restart"]    -> stopServer >> startServer cfg
        ["backlog", n] -> getServerBacklog $ read n
        cmd            -> serverCommand $ unwords cmd
    Left _ -> do
      putStrLn $ "no configuration file found; " ++
                 "writing write default config to " ++ cfgfile
      let defcfg = show (def :: Config)
      res <- try (writeFile cfgfile defcfg) :: IO (Either SomeException ())
      case res of
        Left _ -> error $ "couldn't write default config to " ++ cfgfile
        _      -> return ()
    _ -> do
      error $ "unable to parse " ++ cfgfile

options :: [OptDescr (Maybe FilePath)]
options = [
    Option "c" ["config"] (ReqArg Just "FILE") "Read configuration from FILE."
  ]
