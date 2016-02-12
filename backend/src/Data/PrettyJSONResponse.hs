module Data.PrettyJSONResponse
  ( PrettyJSONResponse
  , toPrettyJSON
  , jsonResponse
  ) where

import qualified  Data.Aeson as Aeson
import            Data.Aeson.Encode.Pretty (encodePretty)
import            Happstack.Server (Response, ToMessage(..))

newtype PrettyJSONResponse = PrettyJSONResponse Aeson.Value

instance ToMessage PrettyJSONResponse where
  toContentType _ = "application/json"
  toMessage (PrettyJSONResponse json) = encodePretty json

toPrettyJSON :: Aeson.ToJSON a => a -> PrettyJSONResponse
toPrettyJSON = PrettyJSONResponse . Aeson.toJSON

jsonResponse :: Aeson.ToJSON a => a -> Response
jsonResponse = toResponse . toPrettyJSON

