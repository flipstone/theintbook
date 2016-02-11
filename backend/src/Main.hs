module Main where

import            Happstack.Server (simpleHTTP, nullConf)
import            System.Environment (getEnv)
import qualified  Database.Redis as Redis

import            Data.JSSource
import            IntBook.Backend (runIntBookBackend)
import            IntBook.Routes (intBookRoutes)

main :: IO ()
main = do
  redisHost <- getEnv "REDIS_HOST"
  jsPath <- getEnv "FRONTEND_JS"

  redisConn <- Redis.connect $ Redis.defaultConnectInfo {
                 Redis.connectHost = redisHost
               }

  jsSource <- newJSSource jsPath
  simpleHTTP nullConf $ runIntBookBackend redisConn (intBookRoutes jsSource)

