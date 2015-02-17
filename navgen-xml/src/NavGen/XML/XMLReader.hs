module NavGen.XML.XMLReader where

import           NavGen.IO               (FileReader)
import qualified Data.ByteString.Lazy as  LB
import           Text.XML.Light.Input    (parseXMLDoc)
import           NavGen.XML.Blaze        (elementToBlaze)
import           Text.Blaze.Html         (Html)

xmlFileReader :: FileReader ([String],Html)
xmlFileReader filename =
  do
    mDoc <- fmap parseXMLDoc $ LB.readFile filename 
    case mDoc of
      Nothing -> error "parseXMLDoc returned Nothing"
      Just doc -> return (Nothing,([],elementToBlaze doc))