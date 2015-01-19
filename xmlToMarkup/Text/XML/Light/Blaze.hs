module Text.XML.Light.Blaze where

import Text.XML.Light.Types (Element(..),
                             Content(..),
                             CData(..))
import Text.Blaze.Html      (Html)
import Text.Blaze.Internal  (customParent,
                             customLeaf,
                             stringTag,
                             string)
import Data.Monoid          (mconcat)

elementToBlaze :: Element 
               -> Html
elementToBlaze (Element name attrs content _) =
  let tag = stringTag $ show name in
  case map contentToBlaze content of
    [] -> customLeaf tag True
    xs -> customParent tag $ mconcat xs

contentToBlaze :: Content
               -> Html
contentToBlaze (Elem e)             = elementToBlaze e
contentToBlaze (Text (CData _ s _)) = string s
contentToBlaze (CRef _) = undefined
