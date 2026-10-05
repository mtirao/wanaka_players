{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Player (findPlayer, findPlayers, insertPlayer, updatePlayer, updateAllFields, updateSkill, toPlayerDTO) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import qualified Hasql.Session as Session
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Rel8
import Prelude hiding (filter, null)

import PlayerDTO

data Player f = Player
    { dateBirth :: Column f Text
    , jersey :: Column f Int64
    , height :: Column f Text
    , erContact :: Column f Text
    , erPhone :: Column f Text
    , hand :: Column f Text      
    , mobile :: Column f Text
    , email :: Column f Text
    , firstName :: Column f Text
    , lastName :: Column f Text
    , block :: Column f Int64
    , defence :: Column f Int64
    , spike :: Column f Int64
    , serve :: Column f Int64
    , skills :: Column f Int64
    , position :: Column f Text
    , team :: Column f Text
    , playerId :: Column f Int64
    } deriving stock (Generic)
      deriving anyclass (Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Player f)

playerSchema :: TableSchema (Player Name)
playerSchema = TableSchema
    { name = "players"
    , columns = Player
        { dateBirth = "date_birth"
        , jersey = "jersey"
        , height = "height"
        , erContact = "er_contacts"
        , erPhone = "er_phone"
        , hand = "hand"    
        , mobile = "mobile"
        , email = "email"
        , firstName = "first_name"
        , lastName = "last_name"
        , block = "block"   
        , defence = "defence"
        , spike = "spike"
        , serve = "serve"
        , skills = "skills"
        , position = "position"
        , team = "team"
        , playerId = "id"
        }
    }

findPlayers :: Pool -> IO (Either P.UsageError [Player Result])
findPlayers pool = do
    let query = select $ do
                    p <- each playerSchema
                    return p
    P.use pool (Session.statement () (run query))

findPlayer :: Pool -> Int64 -> IO (Either P.UsageError [Player Result])
findPlayer pool playerId = do
                            let query = select $ do
                                            p <- each playerSchema
                                            where_ (p.playerId ==. lit playerId)
                                            return p
                            P.use pool (Session.statement () (run query))


-- INSERT
insertPlayer :: PlayerDTO -> Pool -> IO (Either P.UsageError [Int64])
insertPlayer p pool = do
                            P.use pool (Session.statement () (run (insert1 p)))
 
insert1 :: PlayerDTO -> Statement (Query (Expr Int64))
insert1 p = insert $ Insert
            { into = playerSchema
            , rows = values [ Player (lit $ p.datebirth) (lit $ p.jersey) (lit $ p.height) (lit $ p.ercontact) (lit $ p.erphone) (lit $ p.hand) (lit $ p.mobile) (lit $ p.email) (lit $ p.firstName) (lit $ p.lastName) (lit $ p.block) (lit $ p.defence) (lit $ p.spike) (lit $ p.serve) (lit $ p.skills) (lit $ p.position) (lit $ p.team) (nextval "player_id_seq") ]
            , returning = Returning (.playerId)
            , onConflict = Abort
            }


-- UPDATE
updateAllFields :: Int64 -> PlayerDTO -> Pool -> IO (Either P.UsageError [Int64])
updateAllFields id player pool = do
            P.use pool (Session.statement () (run (updateAllFields1 id player)))

updateAllFields1 :: Int64 -> PlayerDTO -> Statement (Query (Expr Int64))
updateAllFields1 id player = update $ Update
        { target = playerSchema
        , from = pure ()
        , set = \_ row -> Player (lit player.datebirth) (lit player.jersey) (lit player.height) (lit player.ercontact) (lit player.erphone) (lit player.hand) (lit player.mobile) (lit player.email) (lit player.firstName) (lit player.lastName) (lit player.block) (lit player.defence) (lit player.spike) (lit player.serve) (lit player.skills) (lit player.position) (lit player.team) (row.playerId)
        , updateWhere = \t ui -> (ui.playerId ==. lit id)
        , returning = Returning (.playerId)
        }

-- UPDATE POINTS

updatePlayer :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Pool -> IO (Either P.UsageError [Int64])
updatePlayer id spike serve block defence pool = do
                        P.use pool (Session.statement () (run (update1 id spike serve block defence)))

update1 :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Statement (Query (Expr Int64))
update1 id spike serve block defence = update $ Update
            { target = playerSchema
            , from = pure ()
            , set = \_ row -> Player row.dateBirth row.jersey row.height row.erContact row.erPhone row.hand row.mobile row.email row.firstName row.lastName (row.block + lit block) (row.defence + lit defence) (row.spike + lit spike) (row.serve + lit serve) row.skills row.position row.team row.playerId
            , updateWhere = \t ui -> (ui.playerId ==. lit id)
            , returning = Returning (.playerId)
            }

updateSkill :: Int64 -> Int64 -> Pool -> IO (Either P.UsageError [Int64])
updateSkill id skill pool = do
                        P.use pool (Session.statement () (run (updateSkill1 id skill)))

updateSkill1 :: Int64 -> Int64 -> Statement (Query (Expr Int64))
updateSkill1 id skill = update $ Update
            { target = playerSchema
            , from = pure ()
            , set = \_ row -> Player row.dateBirth row.jersey row.height row.erContact row.erPhone row.hand row.mobile row.email row.firstName row.lastName row.block row.defence row.spike row.serve (lit skill) row.position row.team row.playerId
            , updateWhere = \t ui -> (ui.playerId ==. lit id)
            , returning = Returning (.playerId)
            }

-- Mapper
toPlayerDTO :: Player (Rel8.Result) -> PlayerDTO
toPlayerDTO player = PlayerDTO
    { datebirth = player.dateBirth
    , jersey = player.jersey
    , height = player.height
    , ercontact = player.erContact
    , erphone = player.erPhone
    , hand = player.hand
    , mobile = player.mobile
    , email = player.email
    , firstName = player.firstName  
    , lastName = player.lastName
    , block = player.block
    , defence = player.defence
    , spike = player.spike
    , serve = player.serve
    , skills = player.skills
    , position = convertPosition player.position
    , team = player.team
    , id = Just player.playerId
    , points = Just (player.spike + player.serve + player.block + player.defence)
    }

convertPosition :: Text -> Text
convertPosition pos = case pos of  
    "Setter" -> "SET"
    "Libero" -> "LIB"
    "Outside Hitter" -> "OH"
    "Middle Blocker" -> "MB"
    "Opposite" -> "OPP"
    _ -> "unknown"