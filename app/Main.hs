module Main where

import Data.Time.Clock(UTCTime)
import Config (Token, getToken, getConfig)
import FreeFeed.Types
import FreeFeed.Api
import Time (getUTCTime, olderMinutes)

allPosts :: Token -> String -> IO [Post]
allPosts token user = go False 0 []
    where go lastPage offset posts' =
            if lastPage then
                return posts'
            else do
                timeline' <- timeline token user offset
                go (isLastPage timeline') (offset + 30) (posts' ++ posts timeline')

main :: IO [DeleteResult]
main = do
    token <- getToken <$> getConfig
    currentTime <- getUTCTime
    allPosts token "30m" >>= mapM (removePost token) . older30mPosts currentTime


older30mPosts :: UTCTime -> [Post] -> [Post]
older30mPosts currentTime =
    filter (\p -> olderMinutes 30 (createdAtUTCTime p) currentTime)
