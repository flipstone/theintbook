module IntBook.Handlers.Success
  ( successResponse
  ) where

import            Data.Aeson ((.=))
import qualified  Data.Aeson as Aeson
import            Happstack.Server (Response)

import            Data.PrettyJSONResponse (jsonResponse)

successResponse :: String -> Response
successResponse msg = jsonResponse $ Aeson.object [ "message" .= msg ]

