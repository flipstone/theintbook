module IntBook.Frontend.AppView
  ( renderAppView
  ) where

import qualified  Data.ByteString.Char8 as BS
import qualified  React.Flux as H

import            IntBook.Frontend.AppAction (AppAction(..))
import            IntBook.Frontend.AppData (AppData(..))
import            IntBook.Frontend.Dispatcher (originate)

renderAppView :: AppData -> H.ReactElementM H.ViewEventHandler ()
renderAppView appData = do
  H.div_ $ do
    H.h1_ "Welcome to the Int Book"

  H.div_ $ do
    H.input_ [ "placeholder" H.$= "Sender"
             , "size" H.@= (10 :: Int)
             , H.onKeyUp $ \evt _ -> originate $ Action_SetSender $ H.target evt "value"
             ]

    H.input_ [ "placeholder" H.$= "Recipient"
             , "size" H.@= (10 :: Int)
             , H.onKeyUp $ \evt _ -> originate $ Action_SetRecipient $ H.target evt "value"
             ]

    H.button_ [H.onClick $ \_ _ -> originate $ Action_SendFriendRequest]
              "Send Friend Request"

  H.div_ $ H.h3_ "Ajax Response"
  H.div_ $ H.elemText $ BS.unpack (apiResponse appData)

