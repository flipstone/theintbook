module IntBook.Frontend.AppData
  ( AppData(..)
  , friendRequestRoute
  ) where

import qualified  Data.ByteString.Char8 as BS

import            IntBook.Backend.Routes (BackendRoute(..))
import            IntBook.Data.IntId (IntId)

data AppData = AppData {
    apiResponse :: BS.ByteString
  , sender :: Maybe IntId
  , recipient :: Maybe IntId
  }

friendRequestRoute :: AppData -> Maybe BackendRoute
friendRequestRoute appData =
  SendFriendRequest <$> sender appData <*> recipient appData

