module Main where

import            Control.Concurrent (forkIO)
import            Control.DeepSeq (NFData)
import            Control.Lens (Lens', (&), (.~), (^.))
import            Control.Monad (void)
import qualified  Data.ByteString.Char8 as BS
import qualified  Data.JSString as JSS
import            Data.Maybe (fromMaybe)
import            Data.Typeable (Typeable)
import            GHC.Generics (Generic)
import            GHCJS.Types (JSVal)
import qualified  JavaScript.Web.XMLHttpRequest as Ajax
import            React.Flux ( ReactView, ReactStore, StoreData(..)
                             , defineControllerView, reactRender
                             , mkStore, alterStore
                             )
import qualified  React.Flux as H
import            History (pushState)

import            IntBook.Backend.Routes (BackendRoute(..), encodeBackendRoute)
import            IntBook.Frontend.AppData ( AppData(..)
                                           , apiResponse
                                           , sender
                                           , recipient
                                           , friendRequestRoute
                                           )
import            IntBook.Data.IntId (IntId, IntIdable(..))

main :: IO ()
main = reactRender "theintbookApp" mainView ()

data AppAction =
    Action_SendFriendRequest
  | Action_SetSender JSVal
  | Action_SetRecipient JSVal
  | Action_SetResponse BS.ByteString
  deriving (Typeable, Generic, NFData)

runAction :: AppAction -> AppData -> IO AppData
runAction Action_SendFriendRequest = actionSendFriendRequest
runAction (Action_SetSender jsVal) = actionSetSender jsVal
runAction (Action_SetRecipient jsVal) = actionSetRecipient jsVal
runAction (Action_SetResponse bs) = actionSetResponse bs

actionSendFriendRequest :: AppData -> IO AppData
actionSendFriendRequest appData = do
  case appData ^. friendRequestRoute of
    Nothing -> pure ()
    Just route -> void $ forkIO $ sendFriendRequest route

  pure appData

sendFriendRequest :: BackendRoute -> IO ()
sendFriendRequest route = do
  response <- backendPost route
  pushState "The Int Book" "/sent"
  alterStore appStore (Action_SetResponse $ fromMaybe "No Response!" response)

actionSetSender :: JSVal -> AppData -> IO AppData
actionSetSender jsVal appData = pure $
  appData & sender .~ (toIntId jsVal)

actionSetRecipient :: JSVal -> AppData -> IO AppData
actionSetRecipient jsVal appData = pure $
  appData & recipient .~ (toIntId jsVal)

actionSetResponse :: BS.ByteString -> AppData -> IO AppData
actionSetResponse bs appData = pure $
  appData & apiResponse .~ bs

instance StoreData AppData where
  type StoreAction AppData = AppAction
  transform = runAction

appStore :: ReactStore AppData
appStore = mkStore $ AppData {
    _apiResponse = ""
  , _sender = Nothing
  , _recipient = Nothing
  }

mainView :: ReactView ()
mainView = defineControllerView "theintbook" appStore $ \appData () -> do
  H.div_ $ do
    H.h1_ "Welcome to the Int Book"

  H.div_ $ do
    H.input_ [ "placeholder" H.$= "Sender"
             , "size" H.@= (10 :: Int)
             , H.onKeyUp $ \evt _ ->
                 [H.SomeStoreAction appStore (Action_SetSender $ H.target evt "value")]
             ]

    H.input_ [ "placeholder" H.$= "Recipient"
             , "size" H.@= (10 :: Int)
             , H.onKeyUp $ \evt _ ->
                 [H.SomeStoreAction appStore (Action_SetRecipient $ H.target evt "value")]
             ]

    H.button_ [H.onClick (\_ _ -> [H.SomeStoreAction appStore Action_SendFriendRequest])]
              "Send Friend Request"

  H.div_ $ H.h3_ "Ajax Response"
  H.div_ $ H.elemText $ BS.unpack (appData ^. apiResponse)

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

