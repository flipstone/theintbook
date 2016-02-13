module Main where

import            Control.DeepSeq (NFData)
import qualified  Data.ByteString.Char8 as BS
import            Data.Maybe (fromMaybe)
import            Data.Typeable (Typeable)
import            GHC.Generics (Generic)
import qualified  JavaScript.Web.XMLHttpRequest as Ajax
import            React.Flux ( ReactView, ReactStore, StoreData(..)
                             , defineControllerView, reactRender
                             , mkStore
                             )
import qualified  React.Flux as H
import            History (pushState)

main :: IO ()
main = reactRender "theintbookApp" mainView ()

data AppData = AppData BS.ByteString
data AppAction = AppAction
  deriving (Show, Typeable, Generic, NFData)

instance StoreData AppData where
  type StoreAction AppData = AppAction
  transform _ appData = do
    response <- sendFriendRequest
    pushState "The Int Book" "/sent"
    pure $ AppData $ fromMaybe "No Response!" response

appStore :: ReactStore AppData
appStore = mkStore (AppData "")

mainView :: ReactView ()
mainView = defineControllerView "theintbook" appStore $ \(AppData rsp) () -> do
  H.div_ $ do
    H.h1_ "Welcome to the Int Book"

  H.div_ $ do
    H.button_ [H.onClick (\_ _ -> [H.SomeStoreAction appStore AppAction])]
              "Send Friend Request"

  H.div_ $ H.h3_ "Ajax Response"
  H.div_ $ H.elemText $ BS.unpack rsp

sendFriendRequest :: IO (Maybe BS.ByteString)
sendFriendRequest =
  fmap Ajax.contents $ Ajax.xhrByteString $ Ajax.Request {
    Ajax.reqMethod = Ajax.POST
  , Ajax.reqURI = "/api/123/friend_request/321/send"
  , Ajax.reqLogin = Nothing
  , Ajax.reqHeaders = []
  , Ajax.reqWithCredentials = False
  , Ajax.reqData = Ajax.NoData
  }

