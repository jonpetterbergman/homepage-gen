module Test where

import Text.XML.Light.Input
import Text.XML.Light.Blaze
import Text.Blaze.Html.Renderer.Pretty

test = fmap (renderHtml . elementToBlaze) $ parseXMLDoc "<fe attr=\"sd\"><node><kek></kek></node></fe>"

teststr = maybe (putStrLn "- Nothing -") putStrLn test