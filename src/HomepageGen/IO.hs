{-# LANGUAGE TupleSections #-}
module HomepageGen.IO where

import           Control.Monad             (filterM,
                                            foldM,
                                            guard)
import           Data.Default              (def)
import           Data.Function             (on)
import           Data.List                 (isPrefixOf,
                                            isSuffixOf,
                                            partition,
                                            sortBy,
                                            groupBy,
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
                                            dropExtension,
                                            splitFileName)
import           System.FilePath.Glob      (Pattern,
                                            compile,
                                            match)
import           Text.Pandoc               (readMarkdown)

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
            -> IO (Lang,String,LocalContent)
readDefault fname =
  do
    contents <- fmap (readMarkdown def) $ readFile fname
    return (fileLang fname,maybe err id $ pageTitle contents,contents)
  where err = error $ "default file: " ++ show fname ++ " doesn't have a title" 

readDefaults :: [Pattern]
             -> FilePath
             -> IO [(Lang,String,LocalContent)]
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
    content <- readFile fname
    return (fileLang fname,content)

maybeRun :: (Monad m,Functor m)
         => m Bool
         -> m a
         -> m (Maybe a)
maybeRun test act =
  test >>= go
  where go False = return Nothing
        go True  = fmap Just act

readFlatten :: [Pattern]
            -> FilePath
            -> IO (Maybe (String,Map Lang String))
readFlatten globs dir =
  let fname = combine dir "flatten" in
   maybeRun (doesFileExist fname) $ do
     hasFlatten        <- doesFileExist fname
     guard                hasFlatten
     baseName          <- readFile fname
     nodenamefiles     <- fmap (filter (\f -> "dirname" `isPrefixOf` f &&
                                        not (or $ map (flip match $ f) globs))) $ 
                                getDirectoryContents dir
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
    return $ if r then (x:ts,fs) else (ts, x:fs)

joinLeaves :: [(IntlLabel,IntlContent)]
           -> NavForest IntlLabel IntlContent
joinLeaves = mapMaybe go . groupBy ((==) `on` (urlname . fst)) . sortBy (compare `on` (urlname . fst)) 
  where go [] = Nothing
        go xs@(h:t) = Just $ Node (Label (urlname $ fst h) (Map.unions $ map (nicename . fst) xs))
                                  (Right $ Map.unions $ map snd xs) []
                        
readDir :: [Pattern]
        -> FilePath
        -> IO IntlSite
readDir globs dir =
  do
    (ixfiles,rest)   <- fmap (partition (isPrefixOf "index") . filter (valid globs)) $ 
                              getDirectoryContents dir
    (k,v)            <- readIndex dir ixfiles
    flatten          <- readFlatten globs dir
    (dirnames,leafnames)     <- partitionM doesDirectoryExist rest
    dirs <- mapM (readDir globs . combine dir) dirnames
    leafs <- mapM (readPage dir) leafnames
    return $ let forest = dirs ++ (joinLeaves leafs) in
             case flatten of
               Nothing -> 
                 Node k (Right v) forest
               Just (basename,nodename) ->
                 Node (k { nicename = nodename }) (Left $ fromMaybe (error $ "cannot find node: " ++ basename) $ find (\n -> basename == urlname (key n)) (dirs ++ (joinLeaves leafs))) undefined

--readNode :: [Pattern]
--         -> FilePath
--         -> IO IntlSite
--readNode globs fname = 
--  do
--    isDir <- doesDirectoryExist fname
--    if isDir then readDir globs fname else readLeaf fname

readLocalSites :: FilePath
               -> IO [LocalSite]
readLocalSites fname =
  do
    globs <- readIgnore fname
    site  <- readDir globs fname
    defs  <- readDefaults globs fname
    return $ localizes site defs