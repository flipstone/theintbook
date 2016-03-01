module IntBook.Frontend.AppData
  ( AppData(..)
  , apiResponse, sender, recipient
  , friendRequestRoute
  ) where

import            Control.Lens (Getter, to, (^.))
import            Control.Lens.TH (makeLenses)
import qualified  Data.ByteString.Char8 as BS

import            IntBook.Backend.Routes (BackendRoute(..))
import            IntBook.Data.IntId (IntId)

data AppData = AppData {
    _apiResponse :: BS.ByteString
  , _sender :: Maybe IntId
  , _recipient :: Maybe IntId
  }

makeLenses ''AppData

friendRequestRoute :: Getter AppData (Maybe BackendRoute)
friendRequestRoute = to $ \appData ->
  SendFriendRequest <$> (appData ^. sender) <*> (appData ^. recipient)

