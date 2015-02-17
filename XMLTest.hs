{-# LANGUAGE OverloadedStrings #-}

module XMLTest where

import           Data.NavZip                    (Level(..),
                                                 NavZip(..))
import           Data.NavTree                   (followValue)
import           NavGen.XML.XMLReader           (xmlFileReader)
import           NavGen.IO                      (readLocalSites,
                                                 writePage,
                                                 copyResource,
                                                 Template)
import           NavGen.Data.Navigation         (allPages)
import qualified Text.Blaze.Html5            as  H
import qualified Text.Blaze.Html5.Attributes as  HA
import           Text.Blaze.Html                (toHtml,
                                                 Html)



blazeWriter :: FileWriter
blazeWriter filename doc =
  -- TODO

testTemplate :: Template ([String],Html) Html
testTemplate (thisLang,otherLangs,nav) =
  H.docTypeHtml $ do
    H.head $ H.title "kek"
    H.body $ do
      H.h1 "kek"
      H.div $ snd $ followValue $ here $ level nav

test :: FilePath
     -> FilePath 
     -> IO ()
test src dst =
  do
    (resources,pages) <- readLocalSites xmlFileReader src
    mapM_ (writePage dst testTemplate) $ allPages pages
    mapM_ (copyResource src dst) resources 
