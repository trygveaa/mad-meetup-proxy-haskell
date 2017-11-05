{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings, ViewPatterns #-}
module Main where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types (status200, status302, status404)
import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU


main :: IO ()
main = do
    let defaultPort = "3000"
    portEnv <- lookupEnv "PORT"
    let port = case readMaybe $ fromMaybe defaultPort portEnv of
            Just x -> x
            Nothing -> error "PORT must be a number"

    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Network.Wai.Request -> (Network.Wai.Response -> IO a) -> IO a
app req respond = do
    response <- case path of
        [] -> return $ redirectRelative req "/events/"
        ["events"] -> meetup req
        ["health"] -> return health
        _ -> return $ notFoundPage
    respond response
    where
        path = case pathInfo req of
            (reverse -> "":x) -> x
            x -> x

redirectRelative :: Network.Wai.Request -> BU.ByteString -> Network.Wai.Response
redirectRelative req path = responseLBS status302 [(hLocation, location)] ""
    where location = B.concat [path, rawQueryString req]

meetup :: Network.Wai.Request -> IO Network.Wai.Response
meetup req =
    let
        statusQuery = case getStatusFromQuery $ queryString req of
            Just x -> "?status=" ++ BU.toString x
            Nothing -> ""
        url = "http://api.meetup.com/Fagkvelder-Itera/events" ++ statusQuery
    in do
        response <- httpLBS $ parseRequest_ url
        return $ responseLBS
            (getResponseStatus response)
            [(hContentType, head $ getResponseHeader hContentType response)]
            (getResponseBody response)

getStatusFromQuery :: [(BU.ByteString, Maybe BU.ByteString)] -> Maybe BU.ByteString
getStatusFromQuery queryList =
    case filter ((== "status") . fst) queryList of
        [] -> Nothing
        x:_ -> snd x

okHealthResult :: HealthResponse
okHealthResult = (HealthResponse { status = Ok, problems = [] })

health :: Network.Wai.Response
health = responseLBS
    status200
    [(hContentType, "application/json")]
    (encode okHealthResult)

notFoundPage :: Network.Wai.Response
notFoundPage = responseLBS
    status404
    [(hContentType, "text/html")]
    "<h1>Page not found</h1>"

data HealthStatus = Ok | Warning | Critical

instance ToJSON HealthStatus where
    toJSON p = case p of
        Ok -> "ok"
        Warning -> "warning"
        Critical -> "critical"

data HealthProblems = HealthProblems
    { name :: Text
    , message :: Text
    , status :: HealthStatus
    } deriving (Generic)

instance ToJSON HealthProblems where
    toEncoding = genericToEncoding defaultOptions

data HealthResponse = HealthResponse
    { status :: HealthStatus
    , problems :: [HealthProblems]
    } deriving (Generic)

instance ToJSON HealthResponse where
    toEncoding = genericToEncoding defaultOptions
