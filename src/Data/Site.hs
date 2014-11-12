{-# LANGUAGE OverloadedStrings #-}
module Data.Site where

import           Control.Monad                    (mplus)
import           Data.Function                    (on)
import qualified Data.List                     as L
import           Data.Map                         (Map)
import qualified Data.Map                      as M
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.IO             as LTIO
import           Text.Blaze.Html                  (Html,toHtml)
import           Text.Blaze.Html.Renderer.Text    (renderHtml)
import qualified Text.Blaze.Html5              as H
import           System.FilePath.Posix            (takeExtension,dropExtension)

type Lang = String

data PathElement =
  PathElement 
    {
      urlname :: String
    , nicenames :: Map Lang Text
    }

data Page =
  Page {
         path :: [PathElement]
       , content :: Map Lang Html
       }

type Site = [Page]

noTranslation :: Lang 
              -> a
noTranslation lang = error $ "No translation found for: " ++ lang

mkTitle :: Lang 
        -> Map Lang Text
        -> [Map Lang Text]
        -> Html
mkTitle lang fallbacks = toHtml . T.intercalate " / " . map go 
  where go s = 
          fromMaybe (noTranslation lang) $ 
          (M.lookup lang s) `mplus` (M.lookup lang fallbacks)

mkPage :: Lang
       -> Map Lang Text
       -> Map Lang Html
       -> Page
       -> (FilePath,LT.Text)
mkPage lang titlefallbacks contentfallbacks page = (fullpath,encodedHtml)
    where fullpath = L.intercalate "/" $ map urlname $ path page
          theTitle = mkTitle lang titlefallbacks $ map nicenames $ path page
          encodedHtml = renderHtml $ H.docTypeHtml $ do
            H.head $ H.title theTitle
            H.body $ do
              H.h1 theTitle
              H.div $ fromMaybe (noTranslation lang) $ 
                      (M.lookup lang $ content page) `mplus` 
                      (M.lookup lang $ contentfallbacks)

fileLang :: FilePath -> Lang
fileLang = go . takeExtension
  where go ".md" = "en"
        go xs@['.',x,y] = [x,y]
        go xs = error $ "Cannot understand language: " ++ xs

dropLangExtension :: FilePath -> FilePath
dropLangExtension filename = 
  case takeExtension filename of
    ".md" -> filename
    ['.',x,y] -> dropExtension filename
    xs -> error $ "Cannot understand language: " ++ xs

groupFiles :: [FilePath] -> [[FilePath]]
groupFiles = L.groupBy ((==) `on` dropLangExtension) . L.sort

readPage :: [FilePath] -> IO Page
readPage filenames = fmap go $ mapM LTIO.readFile filenames
  go