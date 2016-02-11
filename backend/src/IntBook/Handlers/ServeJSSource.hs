module IntBook.Handlers.ServeJSSource
  ( serveJSFile
  ) where

import            Control.Monad.IO.Class (liftIO)
import            Happstack.Server ( Response, toResponse
                                   , ok, internalServerError
                                   )
import qualified  Database.Redis as Redis

import            Data.JSSource (JSSource, minifyJSSource)
import            IntBook.Backend (IntBookBackend, liftRedis)

serveJSFile :: JSSource -> IntBookBackend Response
serveJSFile jsSource = do
  minified <- liftIO $ minifyJSSource jsSource
  ok $ toResponse $ minified

