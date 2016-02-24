module IntBook.Backend.Routes
  ( BackendRoute(..)
  , backendRouter
  , decodeBackendRoute
  , encodeBackendRoute
  ) where

import          Prelude hiding (id, (.))

import          Control.Category (id, (.))
import          Data.ByteString (ByteString)
import          Data.Maybe (fromMaybe)
import          Data.Text (Text)
import          Data.Text.Encoding (encodeUtf8)
import          Text.Boomerang (Boomerang, xmaph, (:-))
import          Text.Boomerang.TH (makeBoomerangs)
import          Web.Routes ( decodePathInfo, encodePathInfo, runSite
                           , formatPathSegments
                           )

import          Web.Routes.Boomerang ( Router, TextsError
                                     , (</>), signed, digits
                                     , boomerangSite
                                     )

import          IntBook.Data.IntId (IntId, IntIdable(..))

data BackendRoute =
    SendFriendRequest IntId IntId
  deriving (Eq, Show)

$(makeBoomerangs ''BackendRoute)

backendRouter :: Router () (BackendRoute :- ())
backendRouter =
  "api" </>
    (rSendFriendRequest . (intId </> "friend_request" </> intId </> "send"))

intId :: Boomerang TextsError [Text] r (IntId :- r)
intId = xmaph (fromMaybe (error "Invalid IntId!") . toIntId)
              (Just . fromIntId)
              (signed digits)


decodeBackendRoute :: ByteString -> Either String BackendRoute
decodeBackendRoute path = runSite "" site (decodePathInfo path)

encodeBackendRoute :: BackendRoute -> ByteString
encodeBackendRoute route = encodeUtf8
                         $ uncurry encodePathInfo
                         $ formatPathSegments site route

site = boomerangSite (\_ url -> url) backendRouter
