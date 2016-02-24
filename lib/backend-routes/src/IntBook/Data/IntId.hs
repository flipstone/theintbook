module IntBook.Data.IntId
  ( IntId
  , IntIdable(..)
  ) where

import            Control.Monad (guard)

import qualified  Data.ByteString.Char8 as BS
import            Data.Char (isDigit)
import            Data.Maybe (fromMaybe)
import            Data.String (IsString(..))
import qualified  Data.Text as T
import qualified  Data.Text.Encoding as Enc

newtype IntId = IntId BS.ByteString
  deriving (Eq, Show)

class IntIdable a where
  toIntId :: a -> Maybe IntId
  fromIntId :: IntId -> a

instance IntIdable BS.ByteString where
  toIntId bytes = do
    guard $ isValid bytes
    pure $ IntId bytes

  fromIntId (IntId bytes) = bytes

instance IntIdable T.Text where
  toIntId = toIntId . Enc.encodeUtf8
  fromIntId = Enc.decodeUtf8 . fromIntId

instance IntIdable String where
  toIntId = toIntId . BS.pack
  fromIntId = BS.unpack . fromIntId

instance IsString IntId where
  fromString s =
      fromMaybe (error msg) $ toIntId s
    where
      msg = "Invalid IntId String for for IsString: " ++ show s

isNegative :: BS.ByteString -> Bool
isNegative "" = False
isNegative bytes = BS.head bytes == '-'

isValid :: BS.ByteString -> Bool
isValid bytes =
  if isNegative bytes
  then isValidPositive $ BS.tail bytes
  else isValidPositive bytes

isValidPositive :: BS.ByteString -> Bool
isValidPositive bytes = not (BS.null bytes)
                     && BS.all isDigit bytes

