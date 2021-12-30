# Haskel from dummies
My main goal for this series of articles is to learn Haskell by writing something useful along the way. I am currently reading the incredibly long [Haskell Book](https://haskellbook.com/progress/), and my progress according to Kindle is 65%, which places me at "Foldable" chapter.

I've decided to code something familiar, something similar to what I'm used to coding in ruby. So it's going to be a simple script working with an api.

## The task
We have a social network with users, groups and posts. And want to create a group with short-lived posts (as in instagram stories), so the post made in this group is deleted after some specified time. Our script will be responsible for finding this posts and deleting them.

I'm going to use [Stack](https://docs.haskellstack.org/en/stable/README/), because I find it the most straightforward and easy to use tool to create a project.
```shell
stack new ff-manager
```

Which will create the `ff-manager` directory with `app/Main.hs` file for our script.
```haskell
-- app/Main.hs
module Main where

import Lib

main :: IO ()
main = someFunc
```

We don't need `import Lib` and `someFunc`, so we can remove it for now. But what do we need? We are going to make some https requests, so we need a lib for that. If it were ruby I would go to [https://www.ruby-toolbox.com/categories/http_clients](https://www.ruby-toolbox.com/categories/http_clients) and select the top one. In Haskell I have to reddit and stackoverflow it. So after some digging I've found this options: [http-conduit](https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md), [wreq](https://hackage.haskell.org/package/wreq), [hreq](https://github.com/epicallan/hreq/blob/master/README.md) and [servant-client](https://docs.servant.dev/en/stable/tutorial/Client.html). Wreq is using lenses, and I'm scared of that for now. Servant has the best reputation in my head, but it's also too complicated. hreq is recommended by Bragilevsky in "Haskell in Action", but I haven't read this book yet, so my choice is http-client with http-conduit. To make it work let's add this dependencies to `package.yaml`:
```yaml
- bytestring
- http-client
- http-client-tls
- http-conduit
- aeson
```
And we are ready to hit our API.

The url for posts published to some group is https://freefeed.net/v2/timelines/:groupname?offset=0&sort=created with authentication header ("x-authentication-token", someToken).
And the response is JSON like this:
```json
{
  "timelines":
  {
    "id": "someUID",
    "name": "Posts",
    "user": "someUID",
    "posts": [ "someUID"],
    "subscribers": []
  },
  "users": [],
  "subscriptions": [],
  "subscribers": [],
  "admins": [],
  "isLastPage": true,
  "posts": [
    {
      "id": "someUID",
      "body": "",
      "commentsDisabled": "0",
      "createdAt": "1640716690088",
      "updatedAt": "1640716690088",
      "commentLikes": 0,
      "ownCommentLikes": 0,
      "omittedCommentLikes": 0,
      "omittedOwnCommentLikes": 0,
      "createdBy": "someUID",
      "postedTo": [ "someUID", "someUID" ],
      "comments": [],
      "attachments": [],
      "likes": [],
      "omittedComments": 0,
      "omittedLikes": 6,
      "backlinksCount": 0
    }
  ],
  "comments": [],
  "attachments":[]
}
```
This is a first page of groups timeline with 30 latest posts. So to get all the posts I'm going to have to query it increasing `offset` param by 30 until `isLastPage` is `true`. 

### Hitting the API

Let's try to get just this first page following the http-client tutorial. First we need to model our data. We've already established that from this whole Timeline json we care about `isLastPage` and `posts` keys only, and for the Post we need just `id` and `createdAt`.

```haskell
data Timeline = Timeline {
    isLastPage :: Bool,
    posts :: [Post]
} deriving Show

data Post = Post {
    uid :: String,
    createdAt :: String
} deriving Show
```

To actually get this data we will have to write `FromJSON` instances fot them. We could derive `Generic` and `FromJSON`, but then we will have to describe all the keys from json in our types.

```haskell
instance FromJSON Timeline where
    parseJSON (Object o) = Timeline <$> (o .: "isLastPage") <*> o .: "posts"
    parseJSON _ = mzero

instance FromJSON Post where
    parseJSON (Object o) = Post <$> (o .: "id") <*> o .: "createdAt"
    parseJSON _ = mzero
```

Our simple timeline request could like this then

```haskell
timeline :: String -> Int -> IO Timeline
timeline group offset = do
    let url = "https://freefeed.net/v2/timelines/" ++ group ++ "?offset=" ++ show offset ++ "&sort=created"
    request <- parseRequest url
    response <- httpJSON $ request
    return (getResponseBody response :: Timeline)
```

Except we need to authenticate it first with token. We have to send the token with "x-authentication-token" header. And `setRequestHeader` receives a bytestring, so let's overload strings:

```haskell
{-# LANGUAGE OverloadedStrings #-}

token = "someToken"

timeline :: String -> Int -> IO Timeline
timeline group offset = do
    let url = concat
                [ "https://freefeed.net/v2/timelines/"
                , group
                , "?offset="
                , show offset
                , "&sort=created"
                ]
    request <- setRequestHeader "x-authentication-token" [token]
            <$> parseRequest url
    response <- httpJSON $ request
    return (getResponseBody response :: Timeline)
```
We can launch repl `stack ghci` and check if it works:
```haskell
λ> timeline "30m" 0
Timeline {isLastPage = True, posts = [Post {uid = "20a00241-a365-4a88-81bd-a93499e82fe2", createdAt = "1640716690088"}]}
```
Yay!
