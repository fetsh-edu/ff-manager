{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Client (requestHeaders)
import Control.Monad (mzero, forM_)
import Data.Time.Clock(UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime, getPOSIXTime)
import Data.Time (diffUTCTime)
import GHC.Generics (Generic)

epochToUTC :: Integral a => a -> UTCTime
epochToUTC = posixSecondsToUTCTime . fromIntegral

token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0eXBlIjoiYXBwLnYxIiwiaWQiOiIzMmE2MjEwNC03ZTk5LTQzY2QtYTE4My1kNDFjMzI1MTYwNmQiLCJpc3N1ZSI6MSwidXNlcklkIjoiNjZhNGIxZjMtNWZhZC00OWViLWIwNzgtZmIxNDRiNGNhYWIxIiwiaWF0IjoxNjQwNDQzMTM1fQ.ErjvpCK8ZwiZzbTqdfSU-2Woex9V_CWzeeQTIQ2eppA"

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

timeline :: String -> Int -> IO Timeline
timeline user offset = do
    nakedRequest <- parseRequest ("https://freefeed.net/v2/timelines/" ++ user ++ "?offset=" ++ show offset ++ "&sort=created")
    response <- httpJSON $ nakedRequest { requestHeaders = [("x-authentication-token", token)] }
    pure (getResponseBody response :: Timeline)

allPosts :: String -> IO [Post]
allPosts user = go False 0 []
    where go lastPage offset posts' =
            if lastPage then
                return posts'
            else do
                timeline' <- timeline user offset
                go (isLastPage timeline') (offset + 30) (posts' ++ posts timeline')

main :: IO ()
main = do
    posts' <- allPosts "30m"
    print posts'
    currentTime <- getPOSIXTime
    let el = older30mPosts (epochSecondsToUTC (round currentTime)) posts'
    a <- mapM removePost el
    print a
    print el

removePost :: Post -> IO DeleteResult
removePost post = do
    nakedRequest <- parseRequest ("DELETE https://freefeed.net/v1/posts/" ++ uid post)
    response <- httpJSON $ nakedRequest { requestHeaders = [("x-authentication-token", token)] }
    pure (getResponseBody response :: DeleteResult)

epochSecondsToUTC :: Int -> UTCTime
epochSecondsToUTC = epochToUTC

older30mPosts :: UTCTime -> [Post] -> [Post]
older30mPosts currentTime =
    filter (\p -> older30m (epochSecondsToUTC (read (createdAt p) `div` 1000)) currentTime)

older30m :: UTCTime -> UTCTime -> Bool
older30m old current = diffUTCTime current old / 60 > 3

instance FromJSON Post where
    parseJSON (Object o) = Post <$> (o .: "id") <*> o .: "createdAt"
    parseJSON _ = mzero

instance FromJSON Timeline where
    parseJSON (Object o) = Timeline <$> (o .: "isLastPage") <*> o .: "posts"
    parseJSON _ = mzero
