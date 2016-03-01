module IntBook.Frontend.AppAction
  ( AppAction(..)
  , runAction
  ) where

import            Control.Concurrent (forkIO)
import            Control.DeepSeq (NFData)
import            Control.Monad (void)
import qualified  Data.ByteString.Char8 as BS
import qualified  Data.JSString as JSS
import            Data.Maybe (fromMaybe)
import            Data.Typeable (Typeable)
import            GHC.Generics (Generic)
import            GHCJS.Types (JSVal)
import qualified  JavaScript.Web.XMLHttpRequest as Ajax

import            History (pushState)

import            IntBook.Backend.Routes (BackendRoute(..), encodeBackendRoute)
import            IntBook.Data.IntId (IntIdable(..))
import            IntBook.Frontend.AppData ( AppData(..)
                                           , apiResponse
                                           , sender
                                           , recipient
                                           , friendRequestRoute
                                           )

data AppAction =
    Action_SendFriendRequest
  | Action_SetSender JSVal
  | Action_SetRecipient JSVal
  | Action_SetResponse BS.ByteString
  deriving (Typeable, Generic, NFData)

sendFriendRequest :: (AppAction -> IO ()) -> BackendRoute -> IO ()
sendFriendRequest update route = do
  response <- backendPost route
  pushState "The Int Book" "/sent"
  update (Action_SetResponse $ fromMaybe "No Response!" response)

backendPost :: BackendRoute -> IO (Maybe BS.ByteString)
backendPost route =
  fmap Ajax.contents $ Ajax.xhrByteString $ Ajax.Request {
    Ajax.reqMethod = Ajax.POST
  , Ajax.reqURI = JSS.pack $ BS.unpack $ encodeBackendRoute $ route
  , Ajax.reqLogin = Nothing
  , Ajax.reqHeaders = []
  , Ajax.reqWithCredentials = False
  , Ajax.reqData = Ajax.NoData
  }


runAction :: (AppAction -> IO ()) -> AppAction -> AppData -> IO AppData
runAction update Action_SendFriendRequest = actionSendFriendRequest update
runAction _ (Action_SetSender jsVal) = actionSetSender jsVal
runAction _ (Action_SetRecipient jsVal) = actionSetRecipient jsVal
runAction _ (Action_SetResponse bs) = actionSetResponse bs

actionSendFriendRequest :: (AppAction -> IO ()) -> AppData -> IO AppData
actionSendFriendRequest update appData = do
  case friendRequestRoute appData of
    Nothing -> pure ()
    Just route -> void $ forkIO $ sendFriendRequest update route

  pure appData

actionSetSender :: JSVal -> AppData -> IO AppData
actionSetSender jsVal appData = pure $
  appData { sender = toIntId jsVal }

actionSetRecipient :: JSVal -> AppData -> IO AppData
actionSetRecipient jsVal appData = pure $
  appData { recipient = toIntId jsVal }

actionSetResponse :: BS.ByteString -> AppData -> IO AppData
actionSetResponse bs appData = pure $
  appData { apiResponse = bs }

