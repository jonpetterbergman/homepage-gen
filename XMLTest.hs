module XMLTest where

import           Text.XML.Light.Input
import           Text.XML.Light.Blaze
import           Text.Blaze.Html.Renderer.Pretty

import           Text.XML.Light.Resource

import qualified Data.ByteString.Lazy as LBS

import Text.XML.Light.Proc   (findElements,
                              findAttr)

test = fmap (renderHtml . elementToBlaze) $ parseXMLDoc "<fe attr=\"sd\"><node><kek></kek></node></fe>"

teststr = maybe (putStrLn "- Nothing -") putStrLn test

testResources filename =
  do
    inf <- LBS.readFile filename
    maybe (putStrLn "- Nothing -") (mapM_ putStrLn) $ fmap referencedResources $ parseXMLDoc inf 