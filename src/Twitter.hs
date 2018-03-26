{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Twitter where


import Data.Aeson (FromJSON)
import Network.HTTP.Conduit
import Network.HTTP.Simple (httpJSON)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Web.Authenticate.OAuth
import Data.ByteString.Char8 (pack)

import GHC.Generics

data Tweet = Tweet {
        id :: Integer
      , text :: String
        } deriving (Generic, Show)

instance FromJSON Tweet

tokens = ("TOKEN",
          "TOKEN")

auth = newOAuth {
        oauthServerName = "api.twitter.com",
        oauthConsumerKey="WSrjpQH8UabXErTrN7WFvpse8",
        oauthConsumerSecret="j7RH9yW0XKL8XTPKU0b0OlkPXnAfu1p2OYy5U15MSYNXyzBwQI"
        }

tweet :: String -> IO ()
tweet t = do
    initReq <- parseRequest "https://api.twitter.com/1.1/statuses/update.json"
    let req = urlEncodedBody [("status",pack t)] initReq
    req' <- (flip (signOAuth auth)) req $ (uncurry newCredential) tokens
    manager <- newManager tlsManagerSettings
    httpLbs req' manager >>= (print . responseStatus)

getTweets :: String -> Int -> MaybeT IO [Tweet]
getTweets userID count = tryGetTweets userID 0 count

tryGetTweets :: String -> Integer -> Int-> MaybeT IO [Tweet]
tryGetTweets userID maxId count = do
    initReq <- liftIO $ parseRequest $
        "https://api.twitter.com/1.1/statuses/user_timeline.json?user_id="
        ++ userID ++ "&count=" ++ (show $ min 180 count)
        ++ (if maxId /= 0 then "&max_id=" ++ (show maxId) else [])
    req <- liftIO $ (flip (signOAuth auth)) initReq $ (uncurry newCredential) tokens
    response <- lift $ httpJSON req
    let _tws = responseBody response
    liftIO $ print $ length _tws
    tws <- if (length _tws) == 0 then mzero else return _tws
    let lastId = Twitter.id $ last tws
    fol <- if count <= 180 then lift $ return []
                 else tryGetTweets userID lastId (count-180)
    return $ tws++fol
