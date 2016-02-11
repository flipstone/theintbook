module Data.JSSource
  ( JSSource, newJSSource, minifyJSSource
  ) where

import            Control.Concurrent (MVar, newMVar, modifyMVar)
import qualified  Data.ByteString.Lazy as LBS
import qualified  Data.Time as Time
import            Happstack.Server ( Response, ToMessage(..)
                                   , simpleHTTP, nullConf
                                   , ok, internalServerError
                                   , dirs, nullDir
                                   )
import            Text.Jasmine (minifyFile)
import            System.Directory (getModificationTime)

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

