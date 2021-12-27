module Main where

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
    deleteResults token "30m" 30 currentTime <> deleteResults token "24h" 1440 currentTime
    where
        deleteResults token group limit currentTime =
            allPosts token group
                >>= mapM (removePost token)
                . filter (\p -> olderMinutes limit (createdAtUTCTime p) currentTime)