{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.Console.GetOpt
import Data.Default
import MCCtl.Frontend
import MCCtl.Backend
import MCCtl.Config

main :: IO ()
main = do
  args <- getArgs
  let (opts, cmds, _) = getOpt Permute options args
      cfg = foldr (.) id opts def
  runCmd cfg cmds

-- | All our command line options.
options :: [OptDescr (GlobalConfig -> GlobalConfig)]
options = [
    Option "c" ["config-dir"]
               (ReqArg (\d c -> c {cfgInstanceDir = d}) "DIR")
               "Read instance configurations from DIR.",
    Option "s" ["server"]
               (ReqArg (\s c -> c {cfgTargetServer = s}) "SERVER")
               ("Action affects the specified server. " ++
                "Affects all servers if unset.")
  ]

-- | Parse non-option command lines and execute any commands.
runCmd :: GlobalConfig -> [String] -> IO ()
runCmd cfg args = do
    case args of
      ["init"]       -> startMCCtlServer cfg
      ["shutdown"]   -> shutdownServer
      ["start"]      -> startServer server
      ["start", s]   -> startServer s
      ["stop"]       -> stopServer server
      ["stop", s]    -> stopServer s
      ["restart"]    -> restartServer server
      ["restart", s] -> restartServer s
      ["log", n]     -> getServerBacklog server $ read n
      ["log", n, s]  -> getServerBacklog s $ read n
      cmd            -> serverCommand server $ unwords cmd
  where
    server = cfgTargetServer cfg
    restartServer name = stopServer name >> startServer name
