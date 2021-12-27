module Config where

import Data.ByteString.Internal (ByteString, packChars)
import System.FilePath ((</>))
import System.Directory (XdgDirectory(XdgConfig), getXdgDirectory)

type Token = ByteString
newtype Config = Config { token :: String } deriving (Read, Show)

configPath :: IO FilePath
configPath = fmap (</> "freefeed.conf") (getXdgDirectory XdgConfig "ff-manager")

getConfig :: IO Config
getConfig = read <$> (readFile =<< configPath)

getToken :: Config -> Token
getToken = packChars . token