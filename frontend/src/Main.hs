module Main where

import            React.Flux ( ReactView, ReactStore, StoreData(..)
                             , defineControllerView, reactRender
                             , mkStore
                             )
import qualified  React.Flux as H

main :: IO ()
main = reactRender "theintbookApp" mainView ()

data AppData = AppData
data AppAction = AppAction

instance StoreData AppData where
  type StoreAction AppData = AppAction
  transform _ appData = pure $ appData

appStore :: ReactStore AppData
appStore = mkStore AppData

mainView :: ReactView ()
mainView = defineControllerView "theintbook" appStore $ \appData () -> do
  H.h1_ "Welcome to the Int Book"
