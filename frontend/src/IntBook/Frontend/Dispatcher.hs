module IntBook.Frontend.Dispatcher
  ( dispatch
  , register
  , originate
  ) where

import            Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import            Control.DeepSeq (NFData(..))
import            Data.Typeable (Typeable, cast)
import            React.Flux ( SomeStoreAction(..)
                             , StoreData(..)
                             , ReactStore
                             , alterStore
                             , mkStore
                             )
import            System.IO.Unsafe (unsafePerformIO)

dispatch :: (Typeable t, NFData t) => t -> IO ()
dispatch = alterStore dispatchStore . Dispatch

register :: (StoreData dat, Typeable (StoreAction dat))
         => ReactStore dat -> IO ()
register store = alterStore dispatchStore (Register (SomeStore store))

originate :: (Typeable t, NFData t) => t -> [SomeStoreAction]
originate t = [SomeStoreAction dispatchStore (Dispatch t)]

data SomeStore = forall dat.
                 (StoreData dat, Typeable (StoreAction dat))
              => SomeStore (ReactStore dat)

newtype DispatchStoreData = DispatchStoreData [SomeStore]

data DispatchStoreAction =
    Register SomeStore
  | forall t. (Typeable t, NFData t) => Dispatch t

instance NFData DispatchStoreAction where
  rnf (Register _) = ()
  rnf (Dispatch a) = rnf a

dispatchStore :: ReactStore DispatchStoreData
dispatchStore = mkStore (DispatchStoreData [])

instance StoreData DispatchStoreData where
  type StoreAction DispatchStoreData = DispatchStoreAction

  transform (Register newStore) (DispatchStoreData stores) =
    pure $ DispatchStoreData (newStore : stores)

  transform (Dispatch t) dat@(DispatchStoreData stores) = do
    mapM_ (tryHandle t) stores
    pure dat

tryHandle :: Typeable t => t -> SomeStore -> IO ()
tryHandle t (SomeStore store) =
  case cast t of
    Just action -> alterStore store action
    Nothing -> pure ()

