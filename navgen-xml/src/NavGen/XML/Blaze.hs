module NavGen.XML.Blaze where

import Text.XML.Light.Types  (Element(..),
                              Content(..),
                              CData(..),
                              Attr(..))
import Text.XML.Light.Output (showQName)
import Text.Blaze.Html       (Html)
import Text.Blaze.Internal   (customAttribute,
                              customParent,
                              customLeaf,
                              stringTag,
                              string,
                              Attribute,
                              (!))
import Data.Monoid           (mconcat)
import Data.String           (fromString)

elementToBlaze :: Element 
               -> Html
elementToBlaze (Element name attrs content _) =
  let tag = stringTag $ showQName name 
      blazeAttrs = mconcat $ map attrToBlaze attrs in
  case map contentToBlaze content of
    [] -> customLeaf tag True ! blazeAttrs 
    xs -> customParent tag $ mconcat xs ! blazeAttrs

attrToBlaze :: Attr
            -> Attribute
attrToBlaze (Attr key val) = 
  let tag = stringTag $ showQName key in     
  customAttribute tag $ fromString val

contentToBlaze :: Content
               -> Html
contentToBlaze (Elem e)             = elementToBlaze e
contentToBlaze (Text (CData _ s _)) = string s
contentToBlaze (CRef _)             = error "CRefs are not handled"
