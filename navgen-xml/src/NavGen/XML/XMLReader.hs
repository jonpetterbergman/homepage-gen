module NavGen.XML.XMLReader where

import           NavGen.IO               (FileReader)
import qualified Data.ByteString.Lazy as  LB
import           Text.XML.Light.Input    (parseXMLDoc)
import           Text.XML.Light.Types    (Element(..),
                                          QName(..))
import           Text.XML.Light.Proc     (elChildren,
                                          strContent)      
import           NavGen.XML.Blaze        (elementToBlaze)
import           Text.Blaze.Html         (Html)
import           Data.Monoid             (mconcat)

xmlFileReader :: FileReader ([String],Html)
xmlFileReader filename =
  do
    mDoc <- fmap parseXMLDoc $ LB.readFile filename 
    case mDoc of
      Nothing -> error "parseXMLDoc returned Nothing"
      Just doc -> return (Just $ findTitle doc,([],
                          mconcat $ map elementToBlaze $ elChildren $ findContents doc))

findTitle :: Element 
          -> String
findTitle el = 
  case match (["navgen"],"title") el of
    [t] -> strContent t
    []  -> error "title not found"
    xs  -> error "more than one title found"

findContents :: Element
             -> Element
findContents el =
  case match (["navgen"],"content") el of
    [e] -> e
    []  -> error "no content node found"
    xs  -> error "more than one content node found"

match :: ([String],String)
      -> Element
      -> [Element]
match (h:t,fin) el | qName (elName el) == h   = concatMap (match (t,fin)) $ elChildren el
match ([] ,fin) el | qName (elName el) == fin = [el]
match _ _ = []