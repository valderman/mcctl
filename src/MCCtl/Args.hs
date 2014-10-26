module MCCtl.Args (parseOpts) where
import Data.List
import Data.Default
import System.Directory
import System.IO.Unsafe
import System.Console.GetOpt
import MCCtl.Config

-- | Parse all command line arguments to produce a config and a list of
--   non-option arguments.
parseOpts :: [String] -> Either String (GlobalConfig, [String])
parseOpts args =
    case foldr (.) id opts def of
      cfg | cfgPrintHelp cfg -> Left $ printHelp helpHeader options
          | null cmds        -> Left $ shortHelpMessage
          | otherwise        -> Right (cfg, cmds)
  where
    (opts, cmds, _) = getOpt Permute options args

-- | All our command line options.
options :: [OptDescr (GlobalConfig -> GlobalConfig)]
options = [
    Option "c" ["config"]
               (ReqArg setCfgPath "CONFIG")
               ("Read instance configuration from CONFIG. If CONFIG is an " ++
               "instance file, that is the only instance available to " ++
               "mcctl. If it is a directory, all instance files therein " ++
               "available to mcctl. Defaults to /etc/mcctl.yaml."),
    Option ""  ["delete-data"]
               (NoArg $ \c -> c {cfgDeleteDataDir = True})
               ("Delete data directory as well when removing an instance."),
    Option ""  ["force"]
               (NoArg $ \c -> c {cfgForce = True})
               ("Perform the requested action, even if it is potentially " ++
                "dangerous."),
    Option "i" ["instance"]
               (ReqArg (\s c -> c {cfgTargetServer = Just s}) "INSTANCE")
               ("Action affects the specified instance. " ++
                "Affects all servers if unset."),
    Option "r" ["resume"]
               (NoArg $ \c -> c {cfgResumeEdit = True})
               ("Resume editing an old, broken config instead of starting " ++
                "from the last known good configuration. " ++
                "Has no effect if there is no broken config to resume."),
    Option ""  ["server-dir"]
               (ReqArg (\s c -> c {cfgServerDirectory = Just s}) "DIR")
               ("Use this server-directory when creating a new instance. " ++
                "Defaults to SERVER_WORKING_DIRECTORY/INSTANCE_NAME."),
    Option "h?" ["help"]
                (NoArg $ \c -> c {cfgPrintHelp = True})
                "Print this help message."
  ]
  where
    setCfgPath path cfg = unsafePerformIO $ do
      isdir <- doesDirectoryExist path
      let p = if isdir then Directory path else File path
      return $ cfg {cfgConfigPath = p}

usageLine :: String
usageLine = "Usage: mcctl [OPTIONS] init|shutdown|start|stop|log N|COMMAND"

shortHelpMessage :: String
shortHelpMessage =
  init $ unlines [usageLine, "Try mcctl --help for more information."]

-- | Help message header.
helpHeader :: String
helpHeader = init $ unlines [
  usageLine,
  "",
  "Recognized commands:",
  "",
  "init",
  "  Initialize mcctl and start any autostarted instances.",
  "",
  "shutdown",
  "  Gracefully shut down all running instances and exit mcctl.",
  "",
  "start [INSTANCE]",
  "  Start INSTANCE, or all instances if none is given.",
  "",
  "stop [INSTANCE]",
  "  Stop INSTANCE, or all running instances if none is given.",
  "",
  "log N [INSTANCE]",
  "  Print the last N lines from INSTANCE's log file.",
  "",
  "edit [INSTANCE]",
  "  Edit the given instance's configuration file using the editor found in",
  "  $EDITOR, or 'nano' if $EDITOR is unset.",
  "  INSTANCE can be omitted if there is only a single instance.",
  "",
  "create INSTANCE [-s DIR]",
  "  Create a new instance called INSTANCE, with DIR for its server data" ++
    " directory.",
  "  DIR defaults to SERVER_WORKING_DIRECTORY/worlddata/INSTANCE if unset.",
  "",
  "delete INSTANCE [--delete-data]",
  "  Delete an instance. Leaves the server data directory intact unless ",
  "  --delete-data is specified.",
  "",
  "list",
  "  List all available instances.",
  "",
  "backup [INSTANCE]",
  "  Back up the specified instance to its configured backup directory, or ",
  "  all running instances if none is given.",
  "",
  "COMMAND [-i INSTANCE]",
  "  Pass COMMAND to the specified instance's Minecraft server process,",
  "  or to all running servers if no instance is given.",
  "",
  "Recognized options:",
  ""]

-- | Produce a help message from a header and a description of the available
--   options.
printHelp :: String -> [OptDescr a] -> String
printHelp hdr = (hdr ++) . ("\n" ++) . unlines . map helpString

helpString :: OptDescr a -> String
helpString (Option short long opt help) =
    shorts ++ longs ++ "\n" ++ formatHelpMessage 80 help
  where
    (longarg, shortarg) =
      case opt of
        NoArg _    -> ("", "")
        ReqArg _ a -> ('=':a, ' ':a)
        OptArg _ a -> ("[=" ++ a ++ "]", " [" ++ a ++ "]")
    shorts =
      case intercalate ", " (map (\c -> ['-',c]) short) of
        s | null s    -> ""
          | otherwise -> s ++ shortarg ++ ", "
    longs =
      case intercalate ", " (map (\s -> "--" ++ s) long) of
        l | null l    -> ""
          | otherwise -> l ++ longarg

-- | Break lines at n chars, add two spaces before each.
formatHelpMessage :: Int -> String -> String
formatHelpMessage chars help =
    unlines . map ("  " ++) . breakLines 0 [] $ words help
  where
    breakLines len ln (w:ws)
      | length w >= chars-2     = w:unwords (reverse ln):breakLines 0 [] ws
      | len+length w >= chars-2 = unwords (reverse ln):breakLines 0 [] (w:ws)
      | otherwise               = breakLines (len+1+length w) (w:ln) ws
    breakLines _ ln _ =
      [unwords $ reverse ln]
