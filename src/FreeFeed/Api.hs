{-# LANGUAGE OverloadedStrings #-}
module FreeFeed.Api where

import Network.HTTP.Simple
import FreeFeed.Types
import Config (Token)

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