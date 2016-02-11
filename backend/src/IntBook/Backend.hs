module IntBook.Backend
  ( IntBookBackend
  , liftRedis
  , runIntBookBackend
  ) where

import            Control.Applicative (Alternative)
import            Control.Monad (MonadPlus)
import            Control.Monad.IO.Class (liftIO, MonadIO)
import            Control.Monad.Reader (ReaderT, runReaderT, ask)
import            Happstack.Server ( ServerPartT, mapServerPartT
                                   , FilterMonad(..), ServerMonad
                                   )
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

