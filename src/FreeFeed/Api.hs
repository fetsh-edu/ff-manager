{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module FreeFeed.Api where

import Network.HTTP.Simple
import Config (Token)
import Data.Time (UTCTime)
import Data.Aeson
import GHC.Generics (Generic)
import Control.Monad (mzero)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

newtype DeleteResult = DeleteResult {
    postStillAvailable :: Bool
} deriving (Show, Generic, FromJSON)

data Post = Post {
    uid :: String,
    createdAt :: UTCTime
} deriving (Show)

data Timeline = Timeline {
    isLastPage :: Bool,
    posts :: [Post]
} deriving (Show)

instance FromJSON Post where
    parseJSON (Object o) = Post <$> (o .: "id") <*> (millisToUTC <$> o .: "createdAt")
    parseJSON _ = mzero

instance FromJSON Timeline where
    parseJSON (Object o) = Timeline <$> (o .: "isLastPage") <*> o .: "posts"
    parseJSON _ = mzero

millisToUTC :: String -> UTCTime
millisToUTC = posixSecondsToUTCTime . (/ 1000) . fromInteger . read

timeline :: Token -> String -> Int -> IO Timeline
timeline token user offset = do
    let url = "https://freefeed.net/v2/timelines/" ++ user ++ "?offset=" ++ show offset ++ "&sort=created"
    request <- authRequest token <$> parseRequest url
    response <- httpJSON request
    return (getResponseBody response)

removePost :: Token -> Post -> IO DeleteResult
removePost token post = do
    let url = "DELETE https://freefeed.net/v1/posts/" ++ uid post
    request <- authRequest token <$> parseRequest url
    response <- httpJSON request
    return (getResponseBody response)

authRequest :: Token -> Request -> Request
authRequest token =
    setRequestHeader "x-authentication-token" [token]