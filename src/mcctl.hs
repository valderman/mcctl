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
      ["edit"]       -> editConfig cfg server
      ["edit", s]    -> editConfig cfg s
      ["create"]     -> createInstance server srvdir
      ["create", s]  -> createInstance s srvdir
      ["delete"]     -> deleteInstance cfg server
      ["delete", s]  -> deleteInstance cfg s
      ["backup"]     -> backupInstance server
      ["backup", s]  -> backupInstance s
      ["list"]       -> listInstances
      ["log", n]     -> getServerBacklog server $ read n
      ["log", n, s]  -> getServerBacklog s $ read n
      cmd            -> serverCommand server $ unwords cmd
  where
    server = maybe "" id $ cfgTargetServer cfg
    srvdir = maybe "" id $ cfgServerDirectory cfg
    restartServer name = stopServer name >> startServer name
