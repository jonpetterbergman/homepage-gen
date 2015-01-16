{-# LANGUAGE TupleSections #-}
module HomepageGen.IO where

import           Control.Monad                   (filterM,
                                                  foldM,
                                                  guard)
import           Data.Char                       (isSpace)
import           Data.Default                    (def)
import           Data.Either			 (rights,
		 				  lefts)
import           Data.Function                   (on)
import           Data.LanguageCodes              (ISO639_1(..))
import qualified Data.LanguageCodes         as    ISO639_1
import           Data.List                       (isPrefixOf,
                                                  isSuffixOf,
                                                  partition,
                                                  sortBy,
                                                  groupBy,
                                                  deleteBy,
                                                  find)
import qualified Data.Map                   as    Map
import           Data.Map                        (Map)
import           Data.Maybe                      (mapMaybe,
                                                  fromMaybe)
import           Data.Monoid                     (Monoid(..))
import qualified Data.Text                  as    T
import qualified Data.Text.Lazy             as    LT
import qualified Data.Text.Lazy.IO          as    LTIO
import           Data.NavTree                    (NavTree(..),
                                                  NavForest)
import           HomepageGen.Data.Navigation     (Navigation,
                                                  relativePath,
						  allPages)
import           HomepageGen.Data.Site           (IntlSite,
                                                  LocalSite,
                                                  localizes,
                                                  IntlLabel,
                                                  IntlContent,
                                                  Label(..),
                                                  LocalContent(..),
                                                  Lang)
import           HomepageGen.Html.Template       (Template)
import           System.Directory                (getDirectoryContents,
                                                  doesDirectoryExist,
                                                  doesFileExist,
                                                  copyFile,
                                                  createDirectoryIfMissing)
import           System.FilePath                 (combine,
                                                  takeExtension,
                                                  takeFileName,
                                                  splitExtension,
                                                  dropExtension,
                                                  splitFileName,
                                                  splitPath,
                                                  joinPath,
                                                  takeDirectory)
import           System.FilePath.Glob            (Pattern,
                                                  compile,
                                                  match)
import           Text.Blaze.Html                 (Html)
import           Text.Blaze.Html.Renderer.Pretty (renderHtml)

import Debug.Trace

type FileReader = (FilePath -> IO (Maybe String,Html))

splitLang :: FilePath
          -> Maybe (FilePath,Lang)
splitLang fname = go $ splitExtension fname
  where go (base,['.',x,y]) = fmap (base,) $ ISO639_1.fromChars x y 
        go _                = Nothing

fileLang :: FilePath 
         -> Maybe Lang
fileLang = (fmap snd) . splitLang

dropLangExtension :: FilePath 
                  -> FilePath
dropLangExtension fname = maybe fname fst $ splitLang fname
    
dropMdExtension :: FilePath
                -> FilePath
dropMdExtension filename | ".md" `isSuffixOf` filename = 
                  reverse $ drop 3 $ reverse filename
                         | otherwise = filename

readResourceOrPage :: FileReader
                   -> FilePath
                   -> FilePath
                   -> IO (Either FilePath (IntlLabel,IntlContent))
readResourceOrPage readFun dir fname =
  case splitLang fname of
    Nothing              -> return $ Left $ combine dir fname
    Just (basename,lang) -> 
      do
        (mtitle,contents) <- readFun $ combine dir fname
        return $ Right $ (Label (dropMdExtension basename) 
                                (maybe Map.empty (Map.singleton lang) mtitle), 
                                Map.singleton lang contents)

readIgnore :: FilePath
           -> IO [Pattern]
readIgnore dir = 
  fmap (map compile . lines) $ readFile $ combine dir ".hpignore"

readDefault :: FileReader
            -> FilePath
            -> IO (Lang,String,LocalContent)
readDefault readFun fname =
  do
    (mtitle,contents) <- readFun fname
    return (maybe lerr id $ fileLang fname,maybe err id mtitle,contents)
  where err = error $ "default file: " ++ show fname ++ " doesn't have a title" 
  	lerr = error $ "default file: " ++ show fname ++ " is not translated"

filterDirectoryContents :: (FilePath -> Bool)
                        -> FilePath
                        -> IO [FilePath]
filterDirectoryContents p = fmap (filter p) . getDirectoryContents

readDefaults :: FileReader 
             -> [Pattern]
             -> FilePath
             -> IO [(Lang,String,LocalContent)]
readDefaults readFun globs dir = 
  let isDefault fname = "default" `isPrefixOf` fname &&
                        not (or $ map (flip match $ fname) globs) in
  do
    dfiles <- filterDirectoryContents isDefault dir
    mapM (readDefault readFun) $ map (combine dir) dfiles

valid :: [Pattern]
      -> FilePath
      -> Bool
valid globs xs = not $ "default.md" `isPrefixOf` xs ||
                       "."          `isPrefixOf` xs ||
                       "dirname"    `isPrefixOf` xs ||
                       "flatten"    ==           xs ||
                       (or $ map (flip match $ xs) globs)

readLeaf :: FileReader 
         -> FilePath
         -> IO (Either FilePath IntlSite)
readLeaf readFun fullpath = 
  let (dir,base) = splitFileName fullpath in
    do
      mpage <- readResourceOrPage readFun dir base
      case mpage of
      	Left  path          -> return $ Left path
        Right (lbl,content) ->
          return $ Right $ Node lbl (Right content) []

readNodeName :: FilePath
            -> IO (Lang,String)
readNodeName fname =
  do
    content <- fmap trim $ readFile fname
    return $ maybe err (,content) $ fileLang fname
  where err = error $ "nodename-file " ++ fname ++ " not translated."

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

readIndex :: FileReader
          -> FilePath
          -> [FilePath]
          -> IO (IntlLabel,IntlContent)
readIndex readFun dir ixfiles =
  do
    ixpages  <- fmap rights $ mapM (readResourceOrPage readFun dir) ixfiles
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
                        
readDir :: FileReader
        -> [Pattern]
        -> FilePath
        -> IO ([FilePath],IntlSite)
readDir readFun globs dir =
  do
    (ixfiles,rest)       <- fmap (partition (isPrefixOf "index")) $ 
                                 filterDirectoryContents (valid globs) dir
    (k,v)                <- readIndex readFun dir ixfiles
    flatten              <- readFlatten globs dir
    (dirnames,leafnames) <- partitionM (doesDirectoryExist . combine dir) rest
    dirs                 <- mapM (readDir readFun globs . combine dir) dirnames
    leafs                <- mapM (readResourceOrPage readFun dir) leafnames
    return $ ((concatMap fst dirs) ++ (lefts leafs),) $ 
      let forest = (map snd dirs) ++ (joinLeaves $ rights leafs) in
      case flatten of
        Nothing -> 
          Node k (Right v) forest
        Just (basename,nodename) ->
          case partition (\n -> basename == urlname (key n)) forest of
            ([],_) -> error $ "cannot find node: " ++ show basename ++ 
                                     " in: " ++ show rest
            (h:t,xs) -> 
              Node (k { nicename = nodename }) (Left h) xs

readLocalSites :: FileReader
               -> FilePath
               -> IO ([FilePath],[(Lang,LocalSite)])
readLocalSites readFun dir =
  do
    globs <- readIgnore dir
    (resources,site)  <- readDir readFun globs dir
    defs  <- readDefaults readFun globs dir
    return $ (resources,localizes site defs)

writePage :: FilePath
          -> Template
          -> (Lang,[Lang],Navigation)
          -> IO ()
writePage dir template page@(lang,langs,nav) =
  let filename = combine dir $ relativePath lang nav in
  do
    createDirectoryIfMissing True $ takeDirectory filename
    writeFile filename $ renderHtml $ template page  

copyResource :: FilePath
             -> FilePath
             -> FilePath
             -> IO ()
copyResource src dst dir = 
  let src' = splitPath src
      dst' = splitPath dst
      dir' = splitPath dir
      fulldst = if src' `isPrefixOf` dir' then 
                   joinPath $ dst' ++ (drop (length src') dir')
                else 
                   error $ show src ++ " is not a prefix of " ++ show dir in
  copyFile dir fulldst
      

dMain :: FileReader
      -> FilePath
      -> FilePath
      -> Template
      -> IO ()
dMain readFun src dst template =
  do
    (resources,pages) <- readLocalSites readFun src
    mapM_ (writePage dst template) $ allPages pages
    mapM_ (copyResource src dst) resources