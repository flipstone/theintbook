module IntBook.Handlers.FriendRequest
  ( sendFriendRequest
  ) where

import            Control.Monad (void)
import qualified  Data.ByteString.Char8 as BS
import qualified  Database.Redis as Redis
import            Happstack.Server (Response, ok)

import            Data.IntId (IntId, IntIdable(..))
import            IntBook.Backend (IntBookBackend, liftRedis)
import            IntBook.Handlers.Error (internalError)
import            IntBook.Handlers.Success (successResponse)

type RedisKey = BS.ByteString

sendFriendRequest :: IntId -> IntId -> IntBookBackend Response
sendFriendRequest requestor requestee = do
  let outbox = friendRequestOutbox requestor
      inbox = friendRequestInbox requestee

  result <- liftRedis $ Redis.multiExec $ do
              void $ Redis.sadd outbox [fromIntId requestee]
              Redis.sadd inbox [fromIntId requestor]

  case result of
    Redis.TxSuccess _ -> ok $ successResponse "Friend request sent."
    Redis.TxAborted -> internalError "Transaction Aborted!"
    Redis.TxError msg -> internalError msg

friendRequestOutbox :: IntId -> RedisKey
friendRequestOutbox intId =
  BS.intercalate ":" [ "intId"
                     , fromIntId intId
                     , "friendRequest"
                     , "outbox"
                     ]

friendRequestInbox :: IntId -> RedisKey
friendRequestInbox intId =
  BS.intercalate ":" [ "intId"
                     , fromIntId intId
                     , "friendRequest"
                     , "inbox"
                     ]

