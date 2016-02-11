module Main where

import            Data.Monoid ((<>))
import qualified  Data.ByteString.Lazy as LBS
import            Control.Concurrent (MVar, newMVar, modifyMVar)
import            Control.Monad.IO.Class (liftIO)
import            Happstack.Server ( ServerPart, Response, ToMessage(..)
                                   , simpleHTTP, nullConf, ok
                                   , dirs, nullDir
                                   )

import            Text.Blaze.Html5 ((!))
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A
import            Text.Jasmine (minifyFile)
import            System.Directory (getModificationTime)
import            System.Environment (lookupEnv)
import            System.Exit (exitWith, ExitCode(ExitFailure))
import qualified  Data.Time as Time

main :: IO ()
main = do
  frontendJS <- lookupEnv "FRONTEND_JS"

  case frontendJS of
    Nothing -> do
      putStrLn "FRONTEND_JS environment variable is not set!"
      exitWith $ ExitFailure 3

    Just jsPath -> do
      jsSource <- newJSSource jsPath
      simpleHTTP nullConf $ app jsSource

app :: JSSource -> ServerPart Response
app jsSource =
     (dirs "/assets/frontend.js" $ serveJSFile jsSource)
  <> indexPage

serveJSFile :: JSSource -> ServerPart Response
serveJSFile jsSource = do
  nullDir
  minified <- liftIO $ minifyJSSource jsSource
  ok $ toResponse $ minified

indexPage :: ServerPart Response
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

