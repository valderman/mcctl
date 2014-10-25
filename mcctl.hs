{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import MCCtl.Frontend
import MCCtl.MiddleEnd
import MCCtl.Config
import MCCtl.Args

main :: IO ()
main = do
  opts <- parseOpts `fmap` getArgs
  case opts of
    Right (cfg, cmds) -> runCmd cfg cmds
    Left msg          -> putStrLn msg

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
