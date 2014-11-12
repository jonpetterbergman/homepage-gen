{-# LANGUAGE OverloadedStrings #-}
module Data.Site where

import           Control.Applicative              ((<$>),(<*>),(*>),(<*),pure)
import           Control.Monad                    (mplus)
import           Data.Attoparsec.Text.Lazy        (Parser,
                                                   takeLazyText,
                                                   endOfLine,
                                                   inClass,
                                                   skipSpace,
                                                   sepBy',
                                                   takeWhile1,
                                                   eitherResult,
                                                   parse)
import           Data.Char                        (isAsciiLower,
                                                   isDigit,
                                                   isAlphaNum)
import           Data.Default                     (def)
import           Data.Function                    (on)
import qualified Data.List                     as L
import           Data.Map                         (Map)
import qualified Data.Map                      as M
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (Monoid(..))
import           Data.Text                        (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.IO             as LTIO
import           Text.Blaze.Html                  (Html,toHtml)
import           Text.Blaze.Html.Renderer.Text    (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Text.Markdown                    (markdown)
import           System.FilePath.Posix            (takeExtension,dropExtension)


type Lang = String

data PathElement =
  PathElement 
    {
      urlname :: String
    , nicenames :: Map Lang Text
    }

instance Monoid PathElement where
  mempty = PathElement "" M.empty
  mappend (PathElement u1 n1) (PathElement u2 n2) =
    PathElement (joinUrl u1 u2) (M.union n1 n2)
      where joinUrl ""  url                = url
            joinUrl url url' | url == url' = url
                             | otherwise   = error $ "url path elements differ: " 
                                             ++ url ++ " /= " ++ url' 

data Page =
  Page {
         path :: [PathElement]
       , content :: Map Lang Html
       }

instance Monoid Page where
  mempty = Page [] M.empty
  mappend (Page p1 c1) (Page p2 c2) =
    Page (zipWith mappend p1 p2) (M.union c1 c2)


data FileStruct =
  FileStruct {
               hasLang :: Lang
             , urlPath :: [Text]
             , nicePath :: [Text]
             , markdownContent :: LT.Text
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

renderPage :: Lang
       -> Map Lang Text
       -> Map Lang Html
       -> Page
       -> (FilePath,LT.Text)
renderPage lang titlefallbacks contentfallbacks page = (fullpath,encodedHtml)
    where fullpath = L.intercalate "/" $ map urlname $ path page
          theTitle = mkTitle lang titlefallbacks $ map nicenames $ path page
          encodedHtml = renderHtml $ H.docTypeHtml $ do
            H.head $ H.title theTitle
            H.body $ do
              H.h1 theTitle
              H.div $ fromMaybe (noTranslation lang) $ 
                      (M.lookup lang $ content page) `mplus` 
                      (M.lookup lang $ contentfallbacks)

fileLang :: FilePath 
         -> Lang
fileLang = go . takeExtension
  where go ".md" = "en"
        go xs@['.',x,y] = [x,y]
        go xs = error $ "Cannot understand language: " ++ xs

dropLangExtension :: FilePath 
                  -> FilePath
dropLangExtension filename = 
  case takeExtension filename of
    ".md" -> filename
    ['.',x,y] -> dropExtension filename
    xs -> error $ "Cannot understand language: " ++ xs

groupFiles :: [FilePath] 
           -> [[FilePath]]
groupFiles = L.groupBy ((==) `on` dropLangExtension) . L.sort

validUrlComponent :: Parser Text
validUrlComponent = 
  takeWhile1 $ \c -> isAsciiLower c || c == '-' || isDigit c

validNiceComponent :: Parser Text
validNiceComponent =
  takeWhile1 $ \c -> isAlphaNum c || inClass " -_" c

parseUrlPath :: Parser [Text]
parseUrlPath = 
  skipSpace >> sepBy' validUrlComponent "/"

parseNicePath :: Parser [Text]
parseNicePath =
  skipSpace >> sepBy' validNiceComponent "/"

parseFileStruct :: Lang -> Parser FileStruct
parseFileStruct lang = 
  FileStruct <$> 
  (pure lang) <*>
  ("urlpath:"  *> parseUrlPath <* endOfLine) <*>
  ("nicepath:" *> parseNicePath <* endOfLine) <*>
  takeLazyText  

readFileStruct :: FilePath 
               -> IO FileStruct
readFileStruct filename =
  do
    contents <- LTIO.readFile filename
    either (\s -> error $ "Error parsing filestruct: " ++ s) 
           return $ 
           eitherResult $ parse (parseFileStruct $ fileLang filename) contents

mkPathElement :: Lang
              -> Text
              -> Text
              -> PathElement
mkPathElement lang url nice = 
  PathElement (T.unpack url) (M.singleton lang nice) 

mkPage :: FileStruct
       -> Page
mkPage fs = Page (zipWith (mkPathElement $ hasLang fs) (urlPath fs) (nicePath fs)) 
            (M.singleton (hasLang fs) (markdown def $ markdownContent fs))

readPage :: [FilePath] -> IO Page
readPage filenames = fmap (mconcat . map mkPage) $ mapM readFileStruct filenames
