{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module FreeFeed.Types where

import Data.Aeson
import GHC.Generics (Generic)
import Control.Monad (mzero)
import Time (epochToUTC)
import Data.Time.Clock (UTCTime)

newtype DeleteResult = DeleteResult { postStillAvailable :: Bool }
  deriving (Show, Generic, FromJSON)

data Post = Post {
    uid :: String,
    createdAt :: String
} deriving (Show)

data Timeline = Timeline {
    isLastPage :: Bool,
    posts :: [Post]
} deriving (Show)

instance FromJSON Post where
    parseJSON (Object o) = Post <$> (o .: "id") <*> o .: "createdAt"
    parseJSON _ = mzero

instance FromJSON Timeline where
    parseJSON (Object o) = Timeline <$> (o .: "isLastPage") <*> o .: "posts"
    parseJSON _ = mzero

createdAtUTCTime :: Post -> UTCTime
createdAtUTCTime p = epochToUTC (read (createdAt p) `div` 1000  :: Int)