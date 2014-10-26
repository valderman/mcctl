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
    cfgConfigPath   :: !ConfigPath,

    -- | Given commands affect this server.
    --   Does not affect init or shutdown.
    cfgTargetServer :: !String,

    -- | Print a help message instead of doing anything else?
    cfgPrintHelp    :: !Bool
  } deriving Show

-- | A complete instance configuration.
data Config = Config {
    instanceName   :: !String,
    globalConfig   :: !GlobalConfig,
    instanceConfig :: !Instance
  } deriving Show

instance Default GlobalConfig where
  def = GlobalConfig {
      cfgConfigPath   = File "/etc/mcctl.yaml",
      cfgTargetServer = "",
      cfgPrintHelp    = False
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
