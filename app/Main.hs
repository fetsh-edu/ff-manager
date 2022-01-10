module Main where

import Config (Token, getToken, getConfig)
import FreeFeed.Api
import Data.Time (UTCTime, NominalDiffTime, diffUTCTime, getCurrentTime)

allPosts :: Token -> String -> IO [Post]
allPosts token user = go False 0 []
    where go lastPage offset posts'
            | lastPage = return posts'
            | otherwise = do
                timeline' <- timeline token user offset
                go (isLastPage timeline') (offset + 30) (posts' ++ posts timeline')

deleteOldPosts :: Token -> String -> NominalDiffTime -> UTCTime -> IO [DeleteResult]
deleteOldPosts token group limit currentTime =
    allPosts token group
        >>= mapM (removePost token)
        . filter (\post -> diffUTCTime currentTime (createdAt post) > limit)

main :: IO [DeleteResult]
main = do
    eitherToken <- (fmap . fmap) getToken getConfig
    case eitherToken of
        Left exc -> fail $ "Could not parse file: " ++ show exc
        Right token ->
            getCurrentTime >>=
                deleteOldPosts token "30m" (30 * 60)
                <> deleteOldPosts token "24h" (24 * 60 * 60)