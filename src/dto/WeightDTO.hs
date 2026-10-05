{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module WeightDTO where

import Data.Aeson
import Data.Text (Text)
import Data.Int (Int32, Int64)

-- Weight

data WeightDTO = WeightDTO
    { position :: Text
    , block :: Int64
    , defence :: Int64
    , spike :: Int64
    , serve :: Int64
    } deriving (Eq, Show)

instance ToJSON WeightDTO where
    toJSON WeightDTO {..} = object [
            "position" .= position,
            "block" .= block,
            "defence" .= defence,
            "spike" .= spike,
            "serve" .= serve
        ]

instance FromJSON WeightDTO where
    parseJSON (Object v) = WeightDTO <$> 
        v .: "position" <*>
        v .: "block" <*>
        v .: "defence" <*>
        v .: "spike" <*>
        v .: "serve"
    parseJSON _ = fail "WeightDTO expects an object"
