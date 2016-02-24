module IntBook.Routes
  ( intBookRoutes
  ) where

import            Data.Monoid ((<>))
import qualified  Data.ByteString.Char8 as BS
import            Happstack.Server (Response, dirs, nullDir, askRq, rqUri)

import            Data.JSSource (JSSource)
import            IntBook.Backend (IntBookBackend)
import            IntBook.Backend.Routes (BackendRoute(..), decodeBackendRoute)
import            IntBook.Handlers.IndexPage (indexPage)
import            IntBook.Handlers.FriendRequest (sendFriendRequest)
import            IntBook.Handlers.PingRedis (pingRedis)
import            IntBook.Handlers.ServeJSSource (serveJSFile)

intBookRoutes :: JSSource -> IntBookBackend Response
intBookRoutes jsSource =
     handleAPIRoute
  <> atPath "/assets/frontend.js" (serveJSFile jsSource)
  <> atPath "/redis/ping" pingRedis
  <> indexPage

atPath :: String -> IntBookBackend a -> IntBookBackend a
atPath pathStr backend = dirs pathStr (nullDir >> backend)

handleAPIRoute :: IntBookBackend Response
handleAPIRoute = do
  rq <- askRq

  case decodeBackendRoute (BS.pack $ rqUri rq) of
    Right route -> dispatchBackendRoute route
    Left _ -> mempty

dispatchBackendRoute :: BackendRoute -> IntBookBackend Response
dispatchBackendRoute route =
  case route of
  SendFriendRequest subject object -> sendFriendRequest subject object

