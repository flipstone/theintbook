module Main
  ( main
  ) where

import qualified  Data.ByteString.Char8 as BS
import qualified  Data.JSString as JSS
import            React.Flux ( ReactView, ReactStore, StoreData(..)
                             , defineControllerView, reactRender
                             , mkStore, alterStore
                             )
import qualified  React.Flux as H

import            IntBook.Frontend.AppAction ( AppAction(..), runAction)
import            IntBook.Frontend.AppData ( AppData(..))
import            IntBook.Data.IntId (IntId, IntIdable(..))

main :: IO ()
main = reactRender "theintbookApp" mainView ()

instance StoreData AppData where
  type StoreAction AppData = AppAction
  transform = runAction (alterStore appStore)

appStore :: ReactStore AppData
appStore = mkStore $ AppData {
    apiResponse = ""
  , sender = Nothing
  , recipient = Nothing
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
  H.div_ $ H.elemText $ BS.unpack (apiResponse appData)

