-- | Calculate paths for the files and directories of instances.
module MCCtl.Paths where
import Data.List
import System.FilePath
import System.Directory
import MCCtl.Config

-- | Path to the instance file described by the given global config and
--   instance name.
instanceFilePath :: GlobalConfig -> String -> FilePath
instanceFilePath cfg name = cfgInstanceDir cfg </> name <.> "yaml"

-- | Data directory of the instance described by the given config.
instanceDirectory :: Config -> FilePath
instanceDirectory cfg = serverDirectory $ instanceConfig cfg

-- | JAR file for the instance described by the given config.
instanceJAR :: Config -> FilePath
instanceJAR cfg = serverJAR $ instanceConfig cfg

-- | Get all instances for the given global config.
getAllInstances :: GlobalConfig -> IO [FilePath]
getAllInstances cfg = do
    fs <- getDirectoryContents (cfgInstanceDir cfg)
    return $ filter isInst $ nub' $ map (dropExtension . takeFileName) fs
  where
    nub' = map head . group . sort
    isInst ""      = False
    isInst ('.':_) = False
    isInst _       = True
