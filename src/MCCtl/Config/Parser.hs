{-# LANGUAGE OverloadedStrings #-}
-- | Parse instances from a YAML file.
module MCCtl.Config.Parser (readInstanceFile, checkInstanceFile) where
import System.Directory
import qualified Data.ByteString as BS
import Data.Yaml
import MCCtl.Config

-- | Read an instance configuration from disk.
readInstanceFile :: FilePath -> IO (Maybe Instance)
readInstanceFile file = do
  exists <- doesFileExist file
  if exists
    then parseInstance <$> BS.readFile file
    else return Nothing

-- | Does the given instance exist and have a valid configuration?
checkInstanceFile :: FilePath -> IO Bool
checkInstanceFile f = do
  minst <- readInstanceFile f
  case minst of
    Just inst -> do
      jars <- getDirectoryContents ("/usr/lib/mcctl/jars")
      return $ serverJAR inst `elem` jars
    _ -> do
      return False

-- | Parse an instance YAML file.
parseInstance :: BS.ByteString -> Maybe Instance
parseInstance bs = do
    case decodeEither bs of
      Right res -> flip parseMaybe res $ \o -> do
        restart <- o .:? "restart" .!= True
        cooldown <- o .:? "restart-cooldown" .!= 3
        initheap <- o .:? "heap-size" .!= 1024
        Instance <$> o .:  "server-directory"
                 <*> o .:? "server-jar" .!= "minecraft_server.1.10.2.jar"
                 <*> o .:? "autostart" .!= True
                 <*> pure (restartInfo restart cooldown)
                 <*> pure initheap
                 <*> o .:? "max-heap-size" .!= initheap
                 <*> o .:? "backup-directory"
                 <*> o .:? "server-properties"
      _ -> do
        Nothing
  where
    restartInfo :: Bool -> Int -> RestartInfo
    restartInfo True cooldown = Restart (fromIntegral cooldown)
    restartInfo False _       = DontRestart
