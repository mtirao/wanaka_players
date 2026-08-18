{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module PlayerDTO where

import Data.Aeson
import Data.Text (Text)
import Data.Int (Int32, Int64)

-- Player

data PlayerDTO = PlayerDTO
    { mobile :: Text
    , email :: Text
    , firstName :: Text
    , lastName :: Text
    , block :: Int64
    , defence :: Int64
    , spike :: Int64
    , serve :: Int64
    , skills :: Int64
    , position :: Text
    , team :: Text
    , id :: Maybe Int64
    } deriving (Eq, Show)
    

instance ToJSON PlayerDTO where
    toJSON PlayerDTO {..} = object [
            "mobile" .= mobile,
            "email" .= email,
            "firstname" .= firstName,
            "lastname" .= lastName,
            "block" .= block,
            "defence" .= defence,
            "spike" .= spike,
            "serve" .= serve,
            "skills" .= skills,
            "position" .= position,
            "team" .= team,
            "id" .= id
        ]

instance FromJSON PlayerDTO where
    parseJSON (Object v) = PlayerDTO <$> 
        v .: "mobile" <*>
        v .: "email" <*>
        v .: "firstname" <*>
        v .: "lastname" <*>
        v .: "block" <*>
        v .: "defence" <*>
        v .: "spike" <*>
        v .: "serve" <*>
        v .: "skills" <*>
        v .: "position" <*>
        v .: "team" <*>
        v .:? "id"
    parseJSON _ = fail "ProfileDTO expects an object"
