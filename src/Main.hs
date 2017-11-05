{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric #-}
module Main where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.ByteString.UTF8 as BU


main :: IO ()
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Network.Wai.Request -> (Network.Wai.Response -> IO a) -> IO a
app req respond = do
    response <- case pathInfo req of
        ["events"] -> meetup req
        ["health"] -> return health
        _ -> return $ notFoundPage
    respond response

meetup :: Network.Wai.Request -> IO Network.Wai.Response
meetup req =
    let
        statusQuery = case getStatusFromQuery $ queryString req of
            Just x -> "statusQuery=" ++ BU.toString x
            Nothing -> ""
        url = "http://api.meetup.com/Fagkvelder-Itera/events?" ++ statusQuery
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
        (_, x):_ -> x

okHealthResult :: HealthResponse
okHealthResult = (HealthResponse { status = Ok, problems = [] })

health :: Network.Wai.Response
health = responseLBS
    status200
    [(hContentType, "application/json")]
    (encode okHealthResult)

notFoundPage :: Network.Wai.Response
notFoundPage = responseLBS status404 [(hContentType, "text/html")] "<h1>Page not found</h1>"

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
