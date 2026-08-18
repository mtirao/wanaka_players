{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (app) where

import qualified Servant as S
import Network.Wai (Application, Middleware, Request(..), getRequestBodyChunk, responseLBS)
import Network.HTTP.Types (status401, status200)
import Network.HTTP.Client
    ( defaultManagerSettings
    , httpLbs
    , newManager
    , parseRequest
    , responseBody
    , responseStatus
    )
import qualified Network.HTTP.Client as HC
import Data.Text (Text)
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

import Hasql.Connection (Connection)
import Hasql.Session (QueryError, Session, run)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)

import PlayerDTO
import PlayerHandlers

type API = "api" S.:> "wanaka" S.:> "player" S.:> S.Capture "id" Int64 S.:> S.Get '[S.JSON] PlayerDTO
    S.:<|> "api" S.:> "wanaka" S.:> "player" S.:> S.Get '[S.JSON] [PlayerDTO]
    S.:<|> "api" S.:> "wanaka" S.:> "player" S.:> S.ReqBody '[S.JSON] PlayerDTO S.:> S.Post '[S.JSON] S.NoContent
    S.:<|> "api" S.:> "wanaka" S.:> "player" S.:> S.Capture "id" Int64 S.:> S.Delete '[S.JSON] S.NoContent
    S.:<|> "api" S.:> "wanaka" S.:> "player" S.:> S.Capture "id" Int64 S.:> S.ReqBody '[S.JSON] PlayerDTO S.:> S.Put '[S.JSON] S.NoContent

server :: Pool -> S.Server API
server pool = getProfileHandler pool
        S.:<|> getProfilesHandler pool
        S.:<|> createProfileHandler pool
        S.:<|> deleteProfileHandler pool
        S.:<|> updateProfileHandler pool

api :: S.Proxy API
api = S.Proxy

-- Application entrypoint
app :: Pool -> Application
app pool = authMiddleware $ loggingMiddleware $ S.serve api (server pool)

-- Middleware and helpers

authMiddleware :: Middleware
authMiddleware app req respond = case lookup "authorization" (requestHeaders req) of
            Just authHeader | BS.isPrefixOf "Bearer " authHeader -> do
                isValid <- validateToken (TE.decodeUtf8 $ BS.drop 7 authHeader)
                if isValid
                    then app req respond
                    else respond $ responseLBS status401 [("Content-Type", "application/json")] "{\"error\":\"Unauthorized\"}"
            _ -> respond $ responseLBS status401 [("Content-Type", "application/json")] "{\"error\":\"Unauthorized\"}"

validateToken :: Text -> IO Bool
validateToken token = do
    manager <- HC.newManager HC.defaultManagerSettings
    req <- HC.parseRequest "http://localhost:3001/api/wanaka/token/validate"
    let request =
            req
                { HC.method = "GET"
                , HC.requestHeaders =
                    [ ("content-Type", "application/json")
                    , ("authorization", BS.append "Bearer " (TE.encodeUtf8 token))
                    , ("x-client-id", "client_credentials")
                    ]
                }
    response <- HC.httpLbs request manager
    putStrLn $ "Status: " ++ show (HC.responseStatus response)
    return (HC.responseStatus response == status200)

loggingMiddleware :: Middleware
loggingMiddleware app req respond = do
    now <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now

    chunksRef <- newIORef []
    let pullBody acc = do
            chunk <- getRequestBodyChunk req
            if B.null chunk
                then return (reverse acc)
                else pullBody (chunk : acc)

    chunks <- pullBody []
    writeIORef chunksRef chunks

    let bodyBS = B.concat chunks
        method = BS.unpack (requestMethod req)
        path = show (pathInfo req)
        hdrs = show (requestHeaders req)
        qs = show (queryString req)
        bodyStr = show bodyBS

    putStrLn $ "[Request] " ++ timeStr ++ " " ++ method ++ " " ++ path
    putStrLn $ "  headers: " ++ hdrs
    putStrLn $ "  query: " ++ qs
    putStrLn $ "  body: " ++ bodyStr

    let req' = req { requestBody = do
                        hs <- readIORef chunksRef
                        case hs of
                            [] -> return B.empty
                            (h:rest) -> do
                                writeIORef chunksRef rest
                                return h
                   }

    app req' respond


