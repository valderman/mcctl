-- | Calculate paths for the files and directories of instances.
module MCCtl.Paths where
import Data.List
import System.FilePath
import System.Directory
import MCCtl.Config

-- | Path to the instance file described by the given global config and
--   instance name. If the instance config is a file rather than a directory,
--   the path to that file is returned.
instanceFilePath :: GlobalConfig -> String -> FilePath
instanceFilePath cfg name =
  case cfgConfigPath cfg of
    Directory dir -> dir </> name <.> "yaml"
    File file     -> file

-- | Data directory of the instance described by the given config.
instanceDirectory :: Config -> FilePath
instanceDirectory cfg = serverDirectory $ instanceConfig cfg

-- | JAR file for the instance described by the given config.
instanceJAR :: Config -> FilePath
instanceJAR cfg = serverJAR $ instanceConfig cfg

-- | Get all instances for the given global config.
getAllInstances :: GlobalConfig -> IO [String]
getAllInstances cfg = do
    fs <- case cfgConfigPath cfg of
            Directory dir -> getDirectoryContents dir
            File file     -> return [file]
    return $ nub' $ map dropExtension $ filter isInst $ map takeFileName fs
  where
    nub' = map head . group . sort
    isInst ""      = False
    isInst ('.':_) = False
    isInst f       = takeExtension f == ".yaml"
