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

data Config = Config {
    -- | The working directory for this instance of the server.
    serverDirectory :: !FilePath,
    -- | The path, absolute or relative to 'serverDirectory', of the JAR file
    --   for this server instance.
    serverJAR       :: !FilePath
  } deriving (Show, Read)

instance Default Config where
  def = Config {
      serverDirectory = "/usr/lib/minecraftd",
      serverJAR = "minecraft_server.jar"
    }
