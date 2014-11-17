{-# LANGUAGE OverloadedStrings #-}
module HomepageGen.Data.Site where

import           Control.Applicative              ((<$>),(<*>),(*>),(<*),pure)
import           Control.Monad                    (mplus,zipWithM)
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
import           Text.Blaze.Html.Renderer.Pretty  (renderHtml)
import qualified Text.Blaze.Html5              as H
import           Text.Markdown                    (markdown)
import           System.Directory                 (createDirectoryIfMissing,
                                                   getDirectoryContents)
import           System.FilePath.Posix            (takeExtension,
                                                   takeExtensions,
                                                   dropExtension,
                                                   splitDirectories,
                                                   joinPath,
                                                   combine)
import           Debug.Trace


type Lang = String

data PathHeader =
  PathHeader {
               urlPath :: [Text]
             , nicePath :: [Text]
             }

data FileStruct a =
  FileStruct {
               hasLang :: Lang
             , header :: a
             , markdownContent :: LT.Text
             }

type Site = Map FilePath ([Map Lang Text],Map Lang Html)

noTranslation :: Lang
              -> a
noTranslation lang = error $ "No translation for " ++ show lang

mkTitle :: Lang 
        -> Map Lang Text
        -> [Map Lang Text]
        -> Html
mkTitle lang fallbacks = toHtml . T.intercalate " / " . map go 
  where go s = 
          fromMaybe (noTranslation lang) $ 
          (M.lookup lang s) `mplus` (M.lookup lang fallbacks)

renderPage :: Map Lang Text
           -> Map Lang Html
           -> FilePath
           -> ([Map Lang Text],Map Lang Html)
           -> Lang
           -> (FilePath,LT.Text)
renderPage titlefallbacks contentfallbacks neutralname (nicenames,content) lang = (fullpath,encodedHtml)
    where fullpath = neutralname ++ "." ++ lang
          theTitle = mkTitle lang titlefallbacks nicenames
          encodedHtml = LT.pack $ renderHtml $ H.docTypeHtml $ do
            H.head $ H.title theTitle
            H.body $ do
              H.h1 theTitle
              H.div $ fromMaybe (noTranslation lang) $ 
                      (M.lookup lang content) `mplus` 
                      (M.lookup lang contentfallbacks)

createDirPath :: FilePath
              -> IO ()
createDirPath fname =
  case splitDirectories fname of
    [] -> return ()
    [_] -> return ()
    xs -> createDirectoryIfMissing True $ joinPath $ init xs

writeRendered :: FilePath
              -> (FilePath,LT.Text)
              -> IO ()
writeRendered dstDir (filename,content) =
  do
    createDirPath fullname
    LTIO.writeFile fullname content
  where fullname = dstDir ++ "/" ++ filename 
 
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

hpFiles :: FilePath 
        -> IO [FilePath]
hpFiles = fmap (filter go) . getDirectoryContents
  where go filename | "notranslation." `L.isPrefixOf` filename = False
                    | otherwise =
                       case takeExtensions filename of
                         ".md"                 -> True
                         ['.','m','d','.',_,_] -> True
                         _                     -> False         

trFiles :: FilePath
        -> IO [FilePath]
trFiles = fmap (filter $ L.isPrefixOf "notranslation.") . getDirectoryContents

validUrlComponent :: Parser Text
validUrlComponent = 
  takeWhile1 $ \c -> isAsciiLower c || c == '-' || c == '.' || isDigit c

validNiceComponent :: Parser Text
validNiceComponent =
  takeWhile1 $ \c -> isAlphaNum c || inClass " -_." c

parseUrlPath :: Parser [Text]
parseUrlPath = 
  skipSpace >> sepBy' validUrlComponent "/"

parseNicePath :: Parser [Text]
parseNicePath =
  skipSpace >> sepBy' validNiceComponent "/"

parsePathHeader :: Parser PathHeader
parsePathHeader = 
  PathHeader <$>
  ("urlpath:"  *> parseUrlPath <* endOfLine) <*>
  ("nicepath:" *> parseNicePath <* endOfLine)

parseTransHeader :: Parser Text
parseTransHeader =
  "nicename:" *> validNiceComponent <* endOfLine

parseFileStruct :: Lang
                -> Parser a 
                -> Parser (FileStruct a)
parseFileStruct lang parseHeader = 
  FileStruct <$> 
  (pure lang) <*>
  parseHeader <*>
  takeLazyText  

readFileStruct :: Parser a
               -> FilePath 
               -> IO (FileStruct a)
readFileStruct p filename =
  do
    contents <- LTIO.readFile filename
    either (\s -> error $ "Error parsing filestruct: " ++ s) 
           return $ 
           eitherResult $ parse (parseFileStruct (fileLang filename) 
                          p) contents

readFallbacks :: FilePath
              -> IO (Map Lang Text,Map Lang Html)
readFallbacks dir =
  do
    allFiles <- trFiles dir
    fmap (foldr go (mempty,mempty)) $ 
         mapM (readFileStruct parseTransHeader) allFiles
  where go fs (titles,contents) = 
          (M.insert (hasLang fs) (header fs) titles,
           M.insert (hasLang fs) (markdown def $ markdownContent fs) contents)

mkSite :: [FileStruct PathHeader]
       -> Site
mkSite = foldr go mempty
  where go fs mp = M.insertWith comb (L.intercalate "/" $ map T.unpack $ urlPath $ header fs)
                                (map (M.singleton (hasLang fs)) $ nicePath $ header fs,
                                 M.singleton (hasLang fs) $ markdown def $ markdownContent fs)
                                mp
        comb (path1,cont1) 
             (path2,cont2) = (zipWith M.union path1 path2,M.union cont1 cont2)

makePages :: [Lang]
          -> FilePath
          -> FilePath
          -> IO ()
makePages langs srcDir dstDir =
  do
    (titleFallbacks,contentFallbacks) <- readFallbacks srcDir    
    allFiles <- hpFiles srcDir
    site <- fmap mkSite $ mapM (readFileStruct parsePathHeader) $ map (combine srcDir) allFiles
    mapM_ (writeRendered dstDir) $ concatMap (\(p,page) -> map (renderPage titleFallbacks contentFallbacks p page) langs) $ M.toList site

