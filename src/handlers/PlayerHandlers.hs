{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}


module PlayerHandlers where

import Servant
import Data.Text (Text)
import Data.Int (Int32, Int64)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Hasql.Session (QueryError)
import PlayerDTO
import Player


getProfileHandler :: Pool -> Int64 -> Handler PlayerDTO
getProfileHandler pool userId =  do
    res <- liftIO $ Player.findPlayer pool userId
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ Player.toPlayerDTO $ head as

getProfilesHandler :: Pool -> Handler [PlayerDTO]
getProfilesHandler pool = do
    res <- liftIO $ Player.findPlayers pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map Player.toPlayerDTO as

createProfileHandler :: Pool -> PlayerDTO -> Handler NoContent
createProfileHandler p pl = do
        res <- liftIO $ Player.insertPlayer pl p
        case res of
            Left _ -> throwError err500
            Right [] -> throwError err403
            Right _ -> return NoContent

deleteProfileHandler :: Pool -> Int64 -> Handler NoContent
deleteProfileHandler _ userId =
    if userId == 0
        then throwError err404
        else pure NoContent

updateProfileHandler :: Pool -> Int64 -> PlayerDTO -> Handler NoContent
updateProfileHandler _ userId _ =
    if userId == 0
        then throwError err404
        else pure NoContent  