{-# LANGUAGE TupleSections #-}
module HomepageGen.IO where

import           Data.Default          (def)
import           Data.List             (isPrefixOf,
                                        isSuffixOf,
                                        partition)
import qualified Data.Map          as   Map
import           Data.Monoid           (Monoid(..))
import qualified Data.Text         as   T
import qualified Data.Text.Lazy    as   LT
import qualified Data.Text.Lazy.IO as   LTIO
import           Data.Tree             (Tree(..))
import           HomepageGen.Data.Site (IntlSite,
                                        IntlPage,
                                        Page(..),
                                        Lang,
                                        appendPages,
                                        joinPages)
import           System.Directory      (getDirectoryContents,
                                        doesDirectoryExist)
import           System.FilePath       (combine,
                                        takeExtension,
                                        takeFileName,
                                        dropExtension,
                                        splitFileName)
import           System.FilePath.Glob  (Pattern,
                                        compile,
                                        match)
import           Text.Pandoc           (readMarkdown)

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

dropMdExtension :: FilePath
                -> FilePath
dropMdExtension filename | ".md" `isSuffixOf` filename = 
                  reverse $ drop 3 $ reverse filename
                         | otherwise = filename

readPage :: FilePath
         -> FilePath
         -> IO IntlPage
readPage dir fname =
  let lang = fileLang fname in
  do
    contents <- readFile $ combine dir fname
    return $ Page (dropMdExtension $ dropLangExtension fname) 
                  (Map.singleton lang $ readMarkdown def contents)

readIgnore :: FilePath
           -> IO [Pattern]
readIgnore dir = fmap (map compile . lines) $ readFile $ combine dir ".hpignore"

valid :: [Pattern]
      -> FilePath
      -> Bool
valid globs xs = not $ "default.md" `isPrefixOf` xs ||
                       "."          `isPrefixOf` xs ||
                       (or $ map (flip match $ xs) globs)

readSingle :: FilePath
           -> IO IntlSite
readSingle fullpath = 
  let (dir,base) = splitFileName fullpath in
  fmap (flip Node $ []) $ readPage dir base

readDir :: [Pattern]
        -> FilePath
        -> IO IntlSite
readDir globs dir =
  do
    (ixs,rest) <- fmap (partition (isPrefixOf "index.md") . filter (valid globs)) $ 
                       getDirectoryContents dir
    ixpages    <- mapM (readPage dir) ixs
    children   <- mapM (readNode globs . combine dir) rest
    case ixpages of
      h:t ->
        return $ Node ((foldr appendPages h t) { urlname = takeFileName dir })
                      (joinPages children)
      _ -> error $ "no index found for " ++ show dir 

readNode :: [Pattern]
         -> FilePath
         -> IO IntlSite
readNode globs fname = 
  do
    isDir <- doesDirectoryExist fname
    if isDir then readDir globs fname else readSingle fname

readSite :: FilePath
         -> IO IntlSite
readSite fname = readIgnore fname >>= ((flip readNode) fname)
 