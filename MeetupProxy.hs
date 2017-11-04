{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric #-}

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.ByteString.UTF8 as BU


main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app req respond = do
    response <- case pathInfo req of
        ["events"] -> meetup req
        ["health"] -> return health
        _ -> return $ notFoundPage
    respond response

meetup req =
    let
        status = case getStatusFromQuery $ queryString req of
            Just x -> "status=" ++ BU.toString x
            Nothing -> ""
        url = "http://api.meetup.com/Fagkvelder-Itera/events?" ++ status
    in do
        response <- httpLBS $ parseRequest_ url
        return $ responseLBS
            (getResponseStatus response)
            [(hContentType, head $ getResponseHeader hContentType response)]
            (getResponseBody response)

getStatusFromQuery :: [(BU.ByteString, Maybe BU.ByteString)] -> Maybe BU.ByteString
getStatusFromQuery queryString =
    case filter (\query -> fst query == "status") queryString of
        [] -> Nothing
        (_, x):_ -> x

okHealthResult = (HealthResponse { status = Ok, problems = [] })
health = responseLBS
    status200
    [(hContentType, "application/json")]
    (encode okHealthResult)

notFoundPage = responseLBS status404 [(hContentType, "text/html")] "<h1>Page not found</p>"

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
