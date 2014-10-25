{-# LANGUAGE OverloadedStrings #-}
-- | Parse instances from a YAML file.
module MCCtl.Config.Parser where
import Control.Applicative
import qualified Data.ByteString as BS
import Data.Yaml
import MCCtl.Config

-- | Parse an instance YAML file.
parseInstance :: BS.ByteString -> Maybe Instance
parseInstance bs = do
    case decodeEither bs of
      Right res -> flip parseMaybe res $ \o -> do
        restart <- o .:? "restart" .!= True
        cooldown <- o .:? "restart-cooldown" .!= 3
        Instance <$> o .:  "server-directory"
                 <*> o .:? "server-jar" .!= "minecraft_server.jar"
                 <*> o .:? "autostart" .!= True
                 <*> pure (restartInfo restart cooldown)
                 <*> o .:? "server-properties"
      _ -> do
        Nothing
  where
    restartInfo :: Bool -> Int -> RestartInfo
    restartInfo True cooldown = Restart (fromIntegral cooldown)
    restartInfo False _       = DontRestart
