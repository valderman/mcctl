{-# LANGUAGE OverloadedStrings #-}
-- | Configuration types and data.
module MCCtl.Config where
import Data.Default
import Data.Time.Clock (NominalDiffTime)
import DBus (InterfaceName, ObjectPath, BusName)

-- | Interface to listen for commands on.
dbusIface :: InterfaceName
dbusIface = "cc.ekblad.mcctl"

-- | Path to DBus object.
dbusObj :: ObjectPath
dbusObj = "/cc/ekblad/mcctl"

-- | Bus name on which to listen for commands.
dbusBus :: BusName
dbusBus = "cc.ekblad.mcctl"

-- | Per instance settings.
data Instance = Instance {
    -- | The working directory for this instance of the server.
    serverDirectory  :: !FilePath,

    -- | The path, absolute or relative to 'serverDirectory', of the JAR file
    --   for this server instance.
    serverJAR        :: !FilePath,

    -- | Start this instance automatically when mcctl starts?
    autostart        :: !Bool,

    -- | Restart the instance if it crashes?
    restartOnFailure :: !RestartInfo,

    -- | Initial heap size for Java process, in megabytes.
    initialHeapSize  :: !Int,

    -- | Maximum heap size for Java process, in megabytes.
    maxHeapSize      :: !Int,
    
    -- | Instance server.properties file.
    serverProperties :: !(Maybe String)
  } deriving Show

-- | Command-line settable configuration.
data GlobalConfig = GlobalConfig {
    -- | File or directory containing instance configurations.
    cfgConfigPath      :: !ConfigPath,

    -- | Given commands affect this server.
    --   Does not affect init or shutdown.
    cfgTargetServer    :: !(Maybe String),

    -- | Print a help message instead of doing anything else?
    cfgPrintHelp       :: !Bool,

    -- | Resume editing a broken config when editing.
    cfgResumeEdit      :: !Bool,

    -- | Delete data directory as well when removing an instance?
    cfgDeleteDataDir   :: !Bool,

    -- | Perform the requested action, even if it is potentially dangerous?
    cfgForce           :: !Bool,

    -- | Server directory for newly created instances.
    cfgServerDirectory :: !(Maybe FilePath)
  } deriving Show

-- | A complete instance configuration.
data Config = Config {
    instanceName   :: !String,
    globalConfig   :: !GlobalConfig,
    instanceConfig :: !Instance
  } deriving Show

instance Default GlobalConfig where
  def = GlobalConfig {
      cfgConfigPath      = File "/etc/mcctl.yaml",
      cfgTargetServer    = Nothing,
      cfgPrintHelp       = False,
      cfgResumeEdit      = False,
      cfgDeleteDataDir   = False,
      cfgForce           = False,
      cfgServerDirectory = Nothing
    }

-- | Instance configuration path.
data ConfigPath
  = File !FilePath      -- ^ Config is a file, describing a single instance.
  | Directory !FilePath -- ^ Config is a directory containing 0 or more
                        --   instances.
    deriving Show

-- | Restart instance on crash?
data RestartInfo
  = Restart !NominalDiffTime -- ^ Restart instance, but only if at least n
                             --   seconds have passed since the last restart.
  | DontRestart              -- ^ Don't restart instance.
    deriving Show

-- | Default contents of newly created instances.
defaultConfig :: FilePath -> String
defaultConfig srvdir = unlines [
  "server-directory:  " ++ srvdir,
  "server-jar:        ../minecraft_server.1.8.jar",
  "autostart:         false",
  "restart:           true",
  "restart-cooldown:  3",
  "heap-size:         1024",
  "max-heap-size:     1024",
  "server-properties: |",
  "  generator-settings=",
  "  op-permission-level=4",
  "  resource-pack-hash=",
  "  allow-nether=true",
  "  level-name=world",
  "  enable-query=false",
  "  allow-flight=false",
  "  announce-player-achievements=true",
  "  server-port=25565",
  "  max-world-size=29999984",
  "  level-type=DEFAULT",
  "  enable-rcon=false",
  "  force-gamemode=false",
  "  level-seed=",
  "  server-ip=",
  "  network-compression-threshold=256",
  "  max-build-height=256",
  "  spawn-npcs=true",
  "  white-list=false",
  "  spawn-animals=true",
  "  snooper-enabled=false",
  "  hardcore=false",
  "  online-mode=true",
  "  resource-pack=",
  "  pvp=true",
  "  difficulty=2",
  "  enable-command-block=false",
  "  player-idle-timeout=0",
  "  gamemode=0",
  "  max-players=20",
  "  max-tick-time=60000",
  "  spawn-monsters=true",
  "  view-distance=10",
  "  generate-structures=true",
  "  motd=Powered by mcctl!"]
