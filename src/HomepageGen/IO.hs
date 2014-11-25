{-# LANGUAGE TupleSections #-}
module HomepageGen.IO where

import           Control.Monad          (filterM)
import           Data.Default           (def)
import           Data.List              (isPrefixOf,
                                         isSuffixOf,
                                         partition)
import qualified Data.Map          as    Map
import           Data.Monoid            (Monoid(..))
import qualified Data.Text         as    T
import qualified Data.Text.Lazy    as    LT
import qualified Data.Text.Lazy.IO as    LTIO
import           Data.NavTree           (NavTree(..))
import           HomepageGen.Data.Site  (IntlSite,
                                         LocalSite,
                                         localizes,
                                         IntlLabel,
                                         IntlContent,
                                         Label(..),
                                         LocalContent(..),
                                         Lang,
                                         pageTitle)
import           System.Directory       (getDirectoryContents,
                                         doesDirectoryExist)
import           System.FilePath        (combine,
                                         takeExtension,
                                         takeFileName,
                                         dropExtension,
                                         splitFileName)
import           System.FilePath.Glob   (Pattern,
                                         compile,
                                         match)
import           Text.Pandoc            (readMarkdown)

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
         -> IO (IntlLabel,IntlContent)
readPage dir fname =
  let lang = fileLang fname in
  do
    contents <- fmap (readMarkdown def) $ readFile $ combine dir fname
    return $ (Label (dropMdExtension $ dropLangExtension fname) 
                    (maybe Map.empty (Map.singleton lang) $ pageTitle contents), 
              Map.singleton lang contents)

readIgnore :: FilePath
           -> IO [Pattern]
readIgnore dir = fmap (map compile . lines) $ readFile $ combine dir ".hpignore"

readDefault :: FilePath
            -> IO (String,LocalContent)
readDefault fname =
  do
    contents <- fmap (readMarkdown def) $ readFile fname
    return (maybe err id $ pageTitle contents,contents)
  where err = error $ "default file: " ++ show fname ++ " doesn't have a title" 

readDefaults :: [Pattern]
             -> FilePath
             -> IO [(String,LocalContent)]
readDefaults globs dir = 
  do
    dfiles <- fmap (filter (\f -> "default.md" `isPrefixOf` f && 
                                  not (or $ map (flip match $ f) globs))) $ 
                   getDirectoryContents dir
    mapM readDefault $ map (combine dir) dfiles

valid :: [Pattern]
      -> FilePath
      -> Bool
valid globs xs = not $ "default.md" `isPrefixOf` xs ||
                       "."          `isPrefixOf` xs ||
                       (or $ map (flip match $ xs) globs)

readLeaf :: FilePath
         -> IO IntlSite
readLeaf fullpath = 
  let (dir,base) = splitFileName fullpath in
    do
      (lbl,content) <- readPage dir base
      return $ Node lbl (Right content) []

readIndex :: [Pattern]
          -> FilePath
          -> IO (IntlLabel,IntlContent)
readIndex globs dir =
  do
    ixfiles <- fmap (filter (isPrefixOf "index") . filter (valid globs)) $
                    getDirectoryContents dir
    ixpages <- mapM (readPage dir) ixfiles
    return (Label (takeFileName dir) 
                  (Map.unions $ map (nicename . fst) ixpages),
                   Map.unions $ map snd ixpages)


readDir :: [Pattern]
        -> FilePath
        -> IO IntlSite
readDir globs dir =
  do
    (k,v)    <- readIndex globs dir
    all   <- fmap (filter (not . isPrefixOf "index") . filter (valid globs)) $ 
                   getDirectoryContents dir
    dirs  <- (filterM doesDirectoryExist all) >>= mapM (readDir globs)
    leafs <- (filterM (fmap not . doesDirectoryExist) all) >>= mapM (readPage dir)
    return $ Node k (Right v) undefined

--readDir :: [Pattern]
--        -> FilePath
--        -> IO IntlSite
--readDir globs dir =
--  do
--    (ixs,rest) <- fmap (partition (isPrefixOf "index.md") . filter (valid globs)) $ 
--                       getDirectoryContents dir
--    ixpages    <- mapM (readPage dir) ixs
--    children   <- mapM (readNode globs . combine dir) rest
--    case ixpages of
--      h:t ->
--        return $ Node ((foldr appendPages h t) { urlname = takeFileName dir })
--                      (joinPages children)
--      _ -> error $ "no index found for " ++ show dir 

--readNode :: [Pattern]
--         -> FilePath
--         -> IO IntlSite
--readNode globs fname = 
--  do
--    isDir <- doesDirectoryExist fname
--    if isDir then readDir globs fname else readLeaf fname

--readLocalSites :: FilePath
--               -> IO [LocalSite]
--readLocalSites fname =
--  do
--    globs <- readIgnore fname
--    site  <- readNode globs fname
--    defs  <- readDefaults globs fname
--    return $ localizes site defs