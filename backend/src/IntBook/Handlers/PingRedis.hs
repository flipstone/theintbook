module IntBook.Handlers.PingRedis
  ( pingRedis
  ) where

import            Happstack.Server ( Response, toResponse
                                   , ok, internalServerError
                                   )
import qualified  Database.Redis as Redis

import            IntBook.Backend (IntBookBackend, liftRedis)

pingRedis :: IntBookBackend Response
pingRedis = do
  result <- liftRedis Redis.ping

  case result of
    Right _ -> ok $ toResponse ("Redis is OK" :: String)
    Left err -> internalServerError $ toResponse (show err)

