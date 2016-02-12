module IntBook.Routes
  ( intBookRoutes
  ) where

import            Data.Monoid ((<>))
import            Happstack.Server (Response, dir, dirs, nullDir, path)

import            Data.IntId (IntId, toIntId)
import            Data.JSSource (JSSource)
import            IntBook.Backend (IntBookBackend)
import            IntBook.Handlers.Error (abortError)
import            IntBook.Handlers.IndexPage (indexPage)
import            IntBook.Handlers.FriendRequest (sendFriendRequest)
import            IntBook.Handlers.PingRedis (pingRedis)
import            IntBook.Handlers.ServeJSSource (serveJSFile)

intBookRoutes :: JSSource -> IntBookBackend Response
intBookRoutes jsSource =
     dir "api" (path (\subjectPath ->
       dir "friend_request" $ path (\objectPath -> do
          subject <- pathToId subjectPath
          object <- pathToId objectPath

          atPath "send" (sendFriendRequest subject object)
        )
     ))
  <> atPath "/assets/frontend.js" (serveJSFile jsSource)
  <> atPath "/redis/ping" pingRedis
  <> indexPage

atPath :: String -> IntBookBackend a -> IntBookBackend a
atPath pathStr backend = dirs pathStr (nullDir >> backend)

pathToId :: String -> IntBookBackend IntId
pathToId pathStr =
  case toIntId pathStr of
  Just intId -> pure intId
  Nothing -> abortError 422 "IntIds must be expressed as a string of digits in base 10"

