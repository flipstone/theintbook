module IntBook.Frontend.AppView
  ( renderAppView
  ) where

import qualified  Data.ByteString.Char8 as BS
import qualified  React.Flux as H

import            IntBook.Frontend.AppAction (AppAction(..))
import            IntBook.Frontend.AppData (AppData(..))

renderAppView :: (AppAction -> handler) -> AppData -> H.ReactElementM handler ()
renderAppView handle appData = do
  H.div_ $ do
    H.h1_ "Welcome to the Int Book"

  H.div_ $ do
    H.input_ [ "placeholder" H.$= "Sender"
             , "size" H.@= (10 :: Int)
             , H.onKeyUp $ \evt _ -> handle $ Action_SetSender $ H.target evt "value"
             ]

    H.input_ [ "placeholder" H.$= "Recipient"
             , "size" H.@= (10 :: Int)
             , H.onKeyUp $ \evt _ -> handle $ Action_SetRecipient $ H.target evt "value"
             ]

    H.button_ [H.onClick $ \_ _ -> handle $ Action_SendFriendRequest]
              "Send Friend Request"

  H.div_ $ H.h3_ "Ajax Response"
  H.div_ $ H.elemText $ BS.unpack (apiResponse appData)

