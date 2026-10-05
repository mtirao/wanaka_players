{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Weight (findWeight, updateWeight, toWeightDTO) where

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

import WeightDTO

data Weight f = Weight
    { position :: Column f Text
    , block :: Column f Int64
    , defence :: Column f Int64
    , spike :: Column f Int64
    , serve :: Column f Int64
    } deriving stock (Generic)
      deriving anyclass (Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Weight f)

weightSchema :: TableSchema (Weight Name)
weightSchema = TableSchema
    { name = "weights"
    , columns = Weight
        { position = "position"
        , block = "block"
        , defence = "defence"
        , spike = "spike"
        , serve = "serve"
        }
    }

findWeight :: Pool -> Text -> IO (Either P.UsageError [Weight Result])
findWeight pool position = do
                            let query = select $ do
                                            p <- each weightSchema
                                            where_ (p.position ==. lit position)
                                            return p
                            P.use pool (Session.statement () (run query))



-- UPDATE
updateWeight :: Text -> Int64 -> Int64 -> Int64 -> Int64 -> Pool -> IO (Either P.UsageError [Text])
updateWeight position spike serve block defence pool = do
                        P.use pool (Session.statement () (run (update1 position spike serve block defence)))

update1 :: Text -> Int64 -> Int64 -> Int64 -> Int64 -> Statement (Query (Expr Text))
update1 position spike serve block defence = update $ Update
            { target = weightSchema
            , from = pure ()
            , set = \_ row -> Weight row.position (row.block + lit block) (row.defence + lit defence) (row.spike + lit spike) (row.serve + lit serve)
            , updateWhere = \t ui -> (ui.position ==. lit position)
            , returning = Returning (.position)
            }


-- Mapper
toWeightDTO :: Weight (Rel8.Result) -> WeightDTO
toWeightDTO weight = WeightDTO
    { position = weight.position
    , block = weight.block
    , defence = weight.defence
    , spike = weight.spike
    , serve = weight.serve
    }
