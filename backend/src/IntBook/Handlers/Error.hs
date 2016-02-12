module IntBook.Handlers.Error
  ( errorResponse
  , abortError
  , internalError
  ) where

import            Data.Aeson ((.=))
import qualified  Data.Aeson as Aeson
import            Happstack.Server (Response, escape', rsCode)

import            Data.PrettyJSONResponse (jsonResponse)
import            IntBook.Backend (IntBookBackend)

internalError :: String -> IntBookBackend Response
internalError = pure . errorResponse 500

abortError :: Int -> String -> IntBookBackend a
abortError code msg = escape' $ errorResponse code msg

errorResponse :: Int -> String -> Response
errorResponse code msg =
  let rsp = jsonResponse $ Aeson.object [ "error" .= msg ]
  in rsp { rsCode = code }

