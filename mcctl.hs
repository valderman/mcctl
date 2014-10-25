{-# LANGUAGE OverloadedStrings #-}
import System.Environment
import System.Directory
import System.IO.Unsafe
import System.Console.GetOpt
import Data.Default
import MCCtl.Frontend
import MCCtl.MiddleEnd
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
               (ReqArg setCfgPath "CONFIG")
               ("Read instance configuration from CONFIG. If CONFIG is an " ++
               "instance file, that is the only instance available to " ++
               "mcctl. If it is a directory, all instance files therein " ++
               "available to mcctl. Defaults to /etc/mcctl.yaml."),
    Option "i" ["instance"]
               (ReqArg (\s c -> c {cfgTargetServer = s}) "INSTANCE")
               ("Action affects the specified instance. " ++
                "Affects all servers if unset.")
  ]
  where
    setCfgPath path cfg = unsafePerformIO $ do
      isdir <- doesDirectoryExist path
      let p = if isdir then Directory path else File path
      return $ cfg {cfgConfigPath = p}

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
