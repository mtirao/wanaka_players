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
import PlayerDTO
import Player
import Weight
import WeightDTO


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

updateAllHandler :: Pool -> Int64 -> PlayerDTO -> Handler NoContent
updateAllHandler p playerId player = do
    res <- liftIO $ Player.updateAllFields playerId player p
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err403
        Right _ -> return NoContent

updateSkillHandler :: Pool -> Int64 -> Text -> Handler NoContent
updateSkillHandler p userId point = do
    res <- liftIO $  case point of 
                        "spike" -> Player.updatePlayer userId 1 0 0 0 p
                        "serve" -> Player.updatePlayer userId 0 1 0 0 p
                        "block" -> Player.updatePlayer userId 0 0 1 0 p
                        "defence" -> Player.updatePlayer userId 0 0 0 1 p
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right _ -> return NoContent  


calculateSkills :: PlayerDTO -> WeightDTO -> IO Int64
calculateSkills player weight = do
    let spikeScore = fromIntegral (player.spike) * (weight.spike)
    let serveScore = fromIntegral (player.serve) * (weight.serve)
    let blockScore = fromIntegral (player.block) * (weight.block)
    let defenceScore = fromIntegral (player.defence) * (weight.defence)
    let totalScore = spikeScore + serveScore + blockScore + defenceScore
    return totalScore

updateSkillsHandler :: Pool -> Int64 -> Handler NoContent
updateSkillsHandler pool userId = do
    res <- liftIO $ Player.findPlayer pool userId
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err400
        Right as -> do 
            let player = Player.toPlayerDTO $ head as 
            weightRes <- liftIO $ Weight.findWeight pool (player.position)
            case weightRes of
                Left _ -> throwError err500
                Right [] -> throwError err404
                Right ws -> do  
                    skills <- liftIO $ calculateSkills player (Weight.toWeightDTO $ head ws)
                    res <- liftIO $ Player.updateSkill userId skills pool
                    case res of
                        Left _ -> throwError err500
                        Right [] -> throwError err404
                        Right _ ->  return NoContent   