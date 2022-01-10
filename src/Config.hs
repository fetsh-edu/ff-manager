{-# LANGUAGE OverloadedStrings #-}
module Config where

import Data.ByteString.Internal (ByteString, packChars)
import System.FilePath ((</>))
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)
import Data.Yaml

type Token = ByteString
newtype Config = Config { getToken :: Token } deriving (Read, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> Config . packChars <$> o .: "token"

configPath :: IO FilePath
configPath = fmap (</> "freefeed.conf") (getXdgDirectory XdgConfig "ff-manager")

getConfig :: IO (Either ParseException Config)
getConfig = do
  decodeFileEither =<< configPath