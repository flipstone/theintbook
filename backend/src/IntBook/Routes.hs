module IntBook.Routes
  ( intBookRoutes
  ) where

import            Data.Monoid ((<>))
import            Happstack.Server (Response, dirs, nullDir)

import            Data.JSSource (JSSource)
import            IntBook.Backend (IntBookBackend)
import            IntBook.Handlers.IndexPage (indexPage)
import            IntBook.Handlers.PingRedis (pingRedis)
import            IntBook.Handlers.ServeJSSource (serveJSFile)

intBookRoutes :: JSSource -> IntBookBackend Response
intBookRoutes jsSource =
     atPath "/assets/frontend.js" (serveJSFile jsSource)
  <> atPath "/redis/ping" pingRedis
  <> indexPage

atPath :: String -> IntBookBackend a -> IntBookBackend a
atPath path backend = dirs path (nullDir >> backend)


