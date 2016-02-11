module IntBook.Handlers.IndexPage
  ( indexPage
  ) where

import            Happstack.Server (Response, toResponse, ok)
import            Text.Blaze.Html5 ((!))
import qualified  Text.Blaze.Html5 as H
import qualified  Text.Blaze.Html5.Attributes as A

import            IntBook.Backend (IntBookBackend)

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

