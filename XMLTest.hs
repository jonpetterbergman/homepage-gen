{-# LANGUAGE OverloadedStrings #-}

module XMLTest where

import           Data.NavZip                     (Level(..),
                                                  NavZip(..),
                                                  ancestors)
import           Data.NavTree                    (followValue,
                                                  key)
import           NavGen.XML.XMLReader            (xmlFileReader)
import           NavGen.IO                       (readLocalSites,
                                                  writePage,
                                                  copyResource,
                                                  Template,
                                                  FileWriter)
import           NavGen.Data.Navigation          (allPages)
import           NavGen.Data.Site                (Label(..))
import qualified Text.Blaze.Html5             as  H
import qualified Text.Blaze.Html5.Attributes  as  HA
import           Text.Blaze.Html                 (toHtml,
                                                  Html,
                                                  (!))
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)
import           Data.List                       (intercalate,
                                                  intersperse)
import           Data.Monoid                     (mconcat)

getTitle :: NavZip (Label a) b
         -> a
getTitle = nicename . key . here . level

nicePath :: NavZip (Label String) a
         -> String
nicePath x = intercalate "/" $ (map getTitle $ reverse $ ancestors x) ++ [getTitle x]

nicePathN :: NavZip (Label String) a
          -> Html
nicePathN x = mconcat $ intersperse "/" $ (map go $ reverse $ ancestors x) ++ [toHtml $ getTitle x]
  where go x = H.a ! HA.href (H.toValue $ urlname $ key $ here $ level x) $ toHtml $ getTitle x

blazeWriter :: FileWriter Html
blazeWriter filename = writeFile filename . renderHtml

nicePathTemplate :: Template ([String],Html) Html
nicePathTemplate (thisLang,otherLangs,nav) =
  let title = nicePath nav in
  H.docTypeHtml $ do
    H.head $ H.title $ H.toHtml title
    H.body $ do
      H.h1 $ nicePathN nav
      H.div $ snd $ followValue $ here $ level nav

testNicePath :: FilePath
             -> FilePath 
             -> IO ()
testNicePath src dst =
  do
    (resources,pages) <- readLocalSites xmlFileReader src
    mapM_ (writePage blazeWriter dst nicePathTemplate) $ allPages pages
    mapM_ (copyResource src dst) resources 

--menu :: NavZip (Label String) a
--     -> Forest (Label String)