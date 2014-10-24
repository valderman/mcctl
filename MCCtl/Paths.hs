-- | Calculate paths for the files and directories of instances.
module MCCtl.Paths where
import System.FilePath
import MCCtl.Config

-- | Path to the instance file described by the given global config and
--   instance name.
instanceFilePath :: GlobalConfig -> String -> FilePath
instanceFilePath cfg name = cfgInstanceDir cfg </> name <.> "instance"

-- | Data directory of the instance described by the given config.
instanceDirectory :: Config -> FilePath
instanceDirectory cfg = serverDirectory $ instanceConfig cfg

-- | JAR file for the instance described by the given config.
instanceJAR :: Config -> FilePath
instanceJAR cfg = serverJAR $ instanceConfig cfg
