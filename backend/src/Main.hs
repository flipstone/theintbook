module Main where

import            Data.Monoid ((<>))
import qualified  Data.ByteString.Lazy as LBS
import            Control.Applicative (Alternative)
import            Control.Concurrent (MVar, newMVar, modifyMVar)
import            Control.Monad (MonadPlus)
import            Control.Monad.IO.Class (liftIO, MonadIO)
import            Control.Monad.Reader (ReaderT, runReaderT, ask)
import            Happstack.Server ( ServerPartT, mapServerPartT
                                   , FilterMonad(..), ServerMonad
                                   , Response, ToMessage(..)
                                   , simpleHTTP, nullConf
                                   , ok, internalServerError
                                   , dirs, nullDir
                                   )

import            Text.Blaze.Html5 ((!))
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A
import            Text.Jasmine (minifyFile)
import            System.Directory (getModificationTime)
import            System.Environment (lookupEnv, getEnv)
import            System.Exit (exitWith, ExitCode(ExitFailure))
import qualified  Data.Time as Time
import qualified  Database.Redis as Redis

newtype IntBookBackend a = IntBookBackend {
    unIntBookBackend :: ServerPartT (ReaderT Redis.Connection IO) a
  } deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MonadIO
             , Monoid
             , ServerMonad
             )

instance FilterMonad a (ServerPartT (ReaderT Redis.Connection IO))
      => FilterMonad a IntBookBackend where
  setFilter = IntBookBackend . setFilter
  composeFilter = IntBookBackend . composeFilter
  getFilter = IntBookBackend . getFilter . unIntBookBackend

liftRedis :: Redis.Redis a -> IntBookBackend a
liftRedis redis = IntBookBackend $ do
  conn <- ask
  liftIO $ Redis.runRedis conn redis

runIntBookBackend :: Redis.Connection -> IntBookBackend a -> ServerPartT IO a
runIntBookBackend conn = mapServerPartT (flip runReaderT conn)
                       . unIntBookBackend

main :: IO ()
main = do
  redisHost <- getEnv "REDIS_HOST"
  frontendJS <- lookupEnv "FRONTEND_JS"

  case frontendJS of
    Nothing -> do
      putStrLn "FRONTEND_JS environment variable is not set!"
      exitWith $ ExitFailure 3

    Just jsPath -> do
      redisConn <- Redis.connect $ Redis.defaultConnectInfo {
                     Redis.connectHost = redisHost
                   }

      jsSource <- newJSSource jsPath
      simpleHTTP nullConf $ runIntBookBackend redisConn (app jsSource)

app :: JSSource -> IntBookBackend Response
app jsSource =
     (dirs "/assets/frontend.js" $ serveJSFile jsSource)
  <> (dirs "/redis/ping" $ pingRedis)
  <> indexPage

pingRedis :: IntBookBackend Response
pingRedis = do
  nullDir
  result <- liftRedis Redis.ping

  case result of
    Right _ -> ok $ toResponse ("Redis is OK" :: String)
    Left err -> internalServerError $ toResponse (show err)

serveJSFile :: JSSource -> IntBookBackend Response
serveJSFile jsSource = do
  nullDir
  minified <- liftIO $ minifyJSSource jsSource
  ok $ toResponse $ minified

indexPage :: IntBookBackend Response
indexPage = ok $ toResponse $ do
  H.docType
  H.html $ do
    H.title "The Int Book"

    H.body $ do
      H.div ! A.id "theintbookApp" $ "Loading...."
      H.script ! A.src "https://fb.me/react-0.14.6.min.js" $ mempty
      H.script ! A.src "https://fb.me/react-dom-0.14.6.min.js" $ mempty
      H.script ! A.src "assets/frontend.js" $ mempty

data JSSource = JSSource {
    jsSourcePath :: FilePath
  , jsSourceMinified :: MVar (JSContent, Time.UTCTime)
  }

newJSSource :: FilePath -> IO JSSource
newJSSource path = do
  mtime <- getModificationTime path
  content <- (JSContent <$> minifyFile path)
  minified <- newMVar (content, mtime)
  pure $ JSSource path minified

minifyJSSource :: JSSource -> IO JSContent
minifyJSSource source =
    modifyMVar (jsSourceMinified source) minifyIfChanged
  where
    minifyIfChanged (content, minifiedAt) = do
      mtime <- getModificationTime $ jsSourcePath source

      if mtime > minifiedAt
        then do
          newContent <- JSContent <$> (minifyFile $ jsSourcePath source)
          pure ((newContent, mtime), newContent)
        else
          pure ((content, minifiedAt), content)

newtype JSContent = JSContent LBS.ByteString

instance ToMessage JSContent where
  toContentType _ = "application/javascript"
  toMessage (JSContent bytes) = bytes

