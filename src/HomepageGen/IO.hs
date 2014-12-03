{-# LANGUAGE TupleSections #-}
module HomepageGen.IO where

import           Control.Monad             (filterM,
                                            foldM,
                                            guard)
import           Data.Char                 (isSpace)
import           Data.Default              (def)
import           Data.Function             (on)
import           Data.LanguageCodes        (ISO639_1(..))
import qualified Data.LanguageCodes   as    ISO639_1
import           Data.List                 (isPrefixOf,
                                            isSuffixOf,
                                            partition,
                                            sortBy,
                                            groupBy,
                                            deleteBy,
                                            find)
import qualified Data.Map             as    Map
import           Data.Map                  (Map)
import           Data.Maybe                (mapMaybe,
                                            fromMaybe)
import           Data.Monoid               (Monoid(..))
import qualified Data.Text            as    T
import qualified Data.Text.Lazy       as    LT
import qualified Data.Text.Lazy.IO    as    LTIO
import           Data.NavTree              (NavTree(..),
                                            NavForest)
import           HomepageGen.Data.Site     (IntlSite,
                                            LocalSite,
                                            localizes,
                                            IntlLabel,
                                            IntlContent,
                                            Label(..),
                                            LocalContent(..),
                                            Lang,
                                            pageTitle)
import           System.Directory          (getDirectoryContents,
                                            doesDirectoryExist,
                                            doesFileExist)
import           System.FilePath           (combine,
                                            takeExtension,
                                            takeFileName,
                                            splitExtension,
                                            dropExtension,
                                            splitFileName)
import           System.FilePath.Glob      (Pattern,
                                            compile,
                                            match)
import           Text.Pandoc               (readMarkdown)

import Debug.Trace

splitLang :: FilePath
          -> (FilePath,Lang)
splitLang fname = go $ splitExtension fname
  where go (base,['.',x,y]) = maybe (fname,ISO639_1.EN) (base,) $ ISO639_1.fromChars x y 
        go _                = (fname,ISO639_1.EN)

fileLang :: FilePath 
         -> Lang
fileLang = snd . splitLang

dropLangExtension :: FilePath 
                  -> FilePath
dropLangExtension = fst . splitLang
    
dropMdExtension :: FilePath
                -> FilePath
dropMdExtension filename | ".md" `isSuffixOf` filename = 
                  reverse $ drop 3 $ reverse filename
                         | otherwise = filename

readPage :: FilePath
         -> FilePath
         -> IO (IntlLabel,IntlContent)
readPage dir fname =
  let (basename,lang) = splitLang fname in
  do
    contents <- fmap (readMarkdown def) $ readFile $ combine dir fname
    return $ (Label (dropMdExtension basename) 
                    (maybe Map.empty (Map.singleton lang) $ 
                           pageTitle contents), 
                    Map.singleton lang contents)

readIgnore :: FilePath
           -> IO [Pattern]
readIgnore dir = 
  fmap (map compile . lines) $ readFile $ combine dir ".hpignore"

readDefault :: FilePath
            -> IO (Lang,String,LocalContent)
readDefault fname =
  do
    contents <- fmap (readMarkdown def) $ readFile fname
    return (fileLang fname,maybe err id $ pageTitle contents,contents)
  where err = error $ "default file: " ++ show fname ++ " doesn't have a title" 

filterDirectoryContents :: (FilePath -> Bool)
                        -> FilePath
                        -> IO [FilePath]
filterDirectoryContents p = fmap (filter p) . getDirectoryContents

readDefaults :: [Pattern]
             -> FilePath
             -> IO [(Lang,String,LocalContent)]
readDefaults globs dir = 
  let isDefault fname = "default.md" `isPrefixOf` fname &&
                        not (or $ map (flip match $ fname) globs) in
  do
    dfiles <- filterDirectoryContents isDefault dir
    mapM readDefault $ map (combine dir) dfiles

valid :: [Pattern]
      -> FilePath
      -> Bool
valid globs xs = not $ "default.md" `isPrefixOf` xs ||
                       "."          `isPrefixOf` xs ||
                       "dirname"    `isPrefixOf` xs ||
                       "flatten"    ==           xs ||
                       (or $ map (flip match $ xs) globs)

readLeaf :: FilePath
         -> IO IntlSite
readLeaf fullpath = 
  let (dir,base) = splitFileName fullpath in
    do
      (lbl,content) <- readPage dir base
      return $ Node lbl (Right content) []

readNodeName :: FilePath
            -> IO (Lang,String)
readNodeName fname =
  do
    content <- fmap trim $ readFile fname
    return (fileLang fname,content)

maybeRun :: (Monad m,Functor m)
         => m Bool
         -> m a
         -> m (Maybe a)
maybeRun test act =
  test >>= go
  where go False = return Nothing
        go True  = fmap Just act

trim :: String
     -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

readFlatten :: [Pattern]
            -> FilePath
            -> IO (Maybe (String,Map Lang String))
readFlatten globs dir =
  let fname           = combine dir "flatten" 
      isDirname fname = "dirname" `isPrefixOf` fname &&
                        not (or $ map (flip match $ fname) globs) in  
  maybeRun (doesFileExist fname) $ do
    baseName          <- fmap trim $ readFile fname
    nodenamefiles     <- filterDirectoryContents isDirname dir
    nodename          <- mapM readNodeName $ map (combine dir) nodenamefiles
    return (baseName,Map.fromList nodename)

readIndex :: FilePath
          -> [FilePath]
          -> IO (IntlLabel,IntlContent)
readIndex dir ixfiles =
  do
    ixpages  <- mapM (readPage dir) ixfiles
    return (Label (takeFileName dir) 
                  (Map.unions $ map (nicename . fst) ixpages),
                   Map.unions $ map snd ixpages)

partitionM :: Monad m 
           => (a -> m Bool) 
           -> [a] 
           -> m ([a],[a])
partitionM p xs = foldM (select p) ([],[]) xs

select :: Monad m 
       => (a -> m Bool) 
       -> ([a], [a]) 
       -> a 
       -> m ([a], [a])
select p (ts,fs) x = 
  do
    r <- p x
    return $ if r then (x:ts,fs) else (ts,x:fs)

joinLeaves :: [(IntlLabel,IntlContent)]
           -> NavForest IntlLabel IntlContent
joinLeaves = mapMaybe go . groupBy ((==) `on` (urlname . fst)) . 
                           sortBy (compare `on` (urlname . fst)) 
  where go [] = Nothing
        go xs@(h:t) = Just $ Node (Label (urlname $ fst h) 
                                         (Map.unions $ map (nicename . fst) xs))
                                  (Right $ Map.unions $ map snd xs) []
                        
readDir :: [Pattern]
        -> FilePath
        -> IO IntlSite
readDir globs dir =
  do
    (ixfiles,rest)       <- fmap (partition (isPrefixOf "index")) $ 
                                 filterDirectoryContents (valid globs) dir
    (k,v)                <- readIndex dir ixfiles
    flatten              <- readFlatten globs dir
    (dirnames,leafnames) <- partitionM (doesDirectoryExist . combine dir) rest
    dirs                 <- mapM (readDir globs . combine dir) dirnames
    leafs                <- mapM (readPage dir) leafnames
    return $ let forest = dirs ++ (joinLeaves leafs) in
             case flatten of
               Nothing -> 
                 Node k (Right v) forest
               Just (basename,nodename) ->
                 case partition (\n -> basename == urlname (key n)) forest of
                   ([],_) -> error $ "cannot find node: " ++ show basename ++ 
                                     " in: " ++ show rest
                   (h:t,xs) -> 
                     Node (k { nicename = nodename }) (Left h) xs

readLocalSites :: FilePath
               -> IO [(Lang,LocalSite)]
readLocalSites fname =
  do
    globs <- readIgnore fname
    site  <- readDir globs fname
    defs  <- readDefaults globs fname
    return $ localizes site defs