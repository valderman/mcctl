{-# LANGUAGE OverloadedStrings #-}
-- | Configuration types and data.
module MCCtl.Config where
import Data.Default
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

    -- | Instance server.properties file.
    serverProperties :: !(Maybe String)
  } deriving (Show, Read)

-- | Command-line settable configuration.
data GlobalConfig = GlobalConfig {
    -- | File or directory containing instance configurations.
    cfgConfigPath   :: !ConfigPath,

    -- | Given commands affect this server.
    --   Does not affect init or shutdown.
    cfgTargetServer :: !String,

    -- | Print a help message instead of doing anything else?
    cfgPrintHelp    :: !Bool
  }

-- | A complete instance configuration.
data Config = Config {
    instanceName   :: !String,
    globalConfig   :: !GlobalConfig,
    instanceConfig :: !Instance
  }

instance Default GlobalConfig where
  def = GlobalConfig {
      cfgConfigPath   = File "/etc/mcctl.yaml",
      cfgTargetServer = "",
      cfgPrintHelp    = False
    }

-- | Instance configuration path; may be either a file or a directory.
data ConfigPath = File FilePath | Directory FilePath
