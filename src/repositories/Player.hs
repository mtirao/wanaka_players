{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Player (findPlayer, findPlayers, insertPlayer, toPlayerDTO) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Internal as TI
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Rel8
import Prelude hiding (filter, null)

import PlayerDTO

data Player f = Player
    { mobile :: Column f Text
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
    , schema = Nothing
    , columns = Player
        { mobile = "mobile"
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
    P.use pool (statement () query)

findPlayer :: Pool -> Int64 -> IO (Either P.UsageError [Player Result])
findPlayer pool playerId = do
                            let query = select $ do
                                            p <- each playerSchema
                                            where_ (p.playerId ==. lit playerId)
                                            return p
                            P.use pool (statement () query)


-- INSERT
insertPlayer :: PlayerDTO -> Pool -> IO (Either P.UsageError [Int64])
insertPlayer p pool = do
                            P.use pool (statement () (insert1 p))

insert1 :: PlayerDTO -> Statement () [Int64]
insert1 p = insert $ Insert
            { into = playerSchema
            , rows = values [ Player (lit $ p.mobile) (lit $ p.email) (lit $ p.firstName) (lit $ p.lastName) (lit $ p.block) (lit $ p.defence) (lit $ p.spike) (lit $ p.serve) (lit $ p.skills) (lit $ p.position) (lit $ p.team) (nextval "player_id_seq") ]
            , returning = Projection (.playerId)
            , onConflict = Abort
            }

-- Mapper
toPlayerDTO :: Player (Rel8.Result) -> PlayerDTO
toPlayerDTO player = PlayerDTO
    { mobile = player.mobile
    , email = player.email
    , firstName = player.firstName  
    , lastName = player.lastName
    , block = player.block
    , defence = player.defence
    , spike = player.spike
    , serve = player.serve
    , skills = player.skills
    , position = player.position
    , team = player.team
    , id = Just player.playerId
    }
