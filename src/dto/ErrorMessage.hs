{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ErrorMessage where

import Data.Aeson
import Data.Text (Text)

-- ErrorMessage
newtype ErrorMessage = ErrorMessage Text
    deriving (Eq, Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]
