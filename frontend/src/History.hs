module History
  ( pushState
  ) where

import            Data.JSString (pack)
import qualified  GHCJS.Types as T

foreign import javascript unsafe
  "history.pushState({}, $1, $2)"
  js_pushState :: T.JSString -> T.JSString -> IO ()


pushState :: String -> String -> IO ()
pushState title url =
  js_pushState (pack title) (pack url)

