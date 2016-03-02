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
import            IntBook.Frontend.Dispatcher (dispatch)

data AppAction =
    Action_SendFriendRequest
  | Action_SetSender JSVal
  | Action_SetRecipient JSVal
  | Action_SetResponse BS.ByteString
  deriving (Typeable, Generic, NFData)

sendFriendRequest :: BackendRoute -> IO ()
sendFriendRequest route = do
  response <- backendPost route
  pushState "The Int Book" "/sent"
  dispatch (Action_SetResponse $ fromMaybe "No Response!" response)

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


runAction :: AppAction -> AppData -> IO AppData
runAction Action_SendFriendRequest = actionSendFriendRequest
runAction (Action_SetSender jsVal) = actionSetSender jsVal
runAction (Action_SetRecipient jsVal) = actionSetRecipient jsVal
runAction (Action_SetResponse bs) = actionSetResponse bs

actionSendFriendRequest :: AppData -> IO AppData
actionSendFriendRequest appData = do
  case friendRequestRoute appData of
    Nothing -> pure ()
    Just route -> void $ forkIO $ sendFriendRequest route

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

