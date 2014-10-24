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
    serverDirectory :: !FilePath,

    -- | The path, absolute or relative to 'serverDirectory', of the JAR file
    --   for this server instance.
    serverJAR       :: !FilePath,

    -- | Start this instance automatically when mcctl starts?
    autostart       :: !Bool
  } deriving (Show, Read)

-- | Command-line settable configuration.
data GlobalConfig = GlobalConfig {
    -- | Directory containing instance configurations.
    cfgInstanceDir  :: !FilePath,

    -- | Given commands affect this server.
    --   Does not affect init or shutdown.
    cfgTargetServer :: !String
  }

-- | A complete instance configuration.
data Config = Config {
    instanceName   :: !String,
    globalConfig   :: !GlobalConfig,
    instanceConfig :: !Instance
  }

instance Default GlobalConfig where
  def = GlobalConfig {
      cfgInstanceDir  = "/etc/mcctl",
      cfgTargetServer = ""
    }
