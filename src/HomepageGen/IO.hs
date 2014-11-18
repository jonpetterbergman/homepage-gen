{-# LANGUAGE TupleSections #-}
module HomepageGen.IO where

import           Data.List             (isPrefixOf,
                                        isSuffixOf,
                                        partition)
import qualified Data.Map          as   Map
import           Data.Monoid           (Monoid(..))
import qualified Data.Text         as   T
import qualified Data.Text.Lazy    as   LT
import qualified Data.Text.Lazy.IO as   LTIO
import           Data.Tree             (Tree(..))
import           HomepageGen.Data.Site (Site,
                                        Page(..),
                                        Lang,
                                        joinPages)
import           System.Directory      (getDirectoryContents,
                                        doesDirectoryExist)
import           System.FilePath       (combine,
                                        takeExtension,
                                        takeFileName,
                                        dropExtension,
                                        splitFileName)

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
         -> IO Page
readPage dir fname =
  let lang = fileLang fname in
  do
    contents <- LTIO.readFile $ combine dir fname
    return $ Page (dropMdExtension $ dropLangExtension fname) 
                  (Map.singleton lang $ T.concat $ LT.toChunks $ 
                                 head $ LT.lines contents) 
                  (Map.singleton lang contents)

valid :: FilePath
      -> Bool
valid ('.':_) = False  
valid xs      = case reverse xs of
                  ('~':_) -> False
                  xs      -> True

readSingle :: FilePath
           -> IO Site
readSingle fullpath = 
  let (dir,base) = splitFileName fullpath in
  fmap (flip Node $ []) $ readPage dir base

readDir :: FilePath
        -> IO Site
readDir dir =
  do
    (ixs,rest) <- fmap (partition (isPrefixOf "index.md") . filter valid) $ 
                       getDirectoryContents dir
    ixpages    <- mapM (readPage dir) ixs
    children   <- mapM (readSite . combine dir) rest
    return $ Node ((mconcat ixpages) { urlname = takeFileName dir })
                  (joinPages children)

readSite :: FilePath
         -> IO Site
readSite fname = 
  do
    isDir <- doesDirectoryExist fname
    if isDir then readDir fname else readSingle fname
