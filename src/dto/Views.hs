module Views where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (ByteString)

jsonResponse :: ToJSON a => a -> ByteString
jsonResponse = encode