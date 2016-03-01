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

import            IntBook.Frontend.AppAction (AppAction(..), runAction)
import            IntBook.Frontend.AppData (AppData(..))
import            IntBook.Frontend.AppView (renderAppView)
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

handleAction :: AppAction -> [H.SomeStoreAction]
handleAction action = [H.SomeStoreAction appStore action]

mainView :: ReactView ()
mainView = defineControllerView "theintbook" appStore $ \appData () ->
  renderAppView handleAction appData

