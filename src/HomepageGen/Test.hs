module HomepageGen.Test where

import HomepageGen.IO               (readLocalSites)
import HomepageGen.Data.Site        (urlname,
                                     Label(..))
import HomepageGen.Data.Navigation  (fromSite,
                                     relativePath)
--                                     pageTitle,
--                                     logicalPath,
--                                     Navigation,
--                                     menu)
import Data.NavTree                 (NavTree,
                                     ppTree,
                                     mapValues,
                                     mapKeys)
--import Data.Tree.Zipper             (TreePos,
--                                     Full(..),
--                                     fromTree,
--                                     firstChild,
--                                     next)
import qualified Data.LanguageCodes as ISO639_1


printTree :: (Show k,Show b)
          => NavTree k b
          -> IO ()
printTree = ppTree . mapKeys show . mapValues show

testUrlNames :: FilePath
             -> IO ()
testUrlNames src =
  do
    sites <- readLocalSites src
    mapM_ printTree $ map (mapValues (const "-")) $ map (mapKeys (\(Label k v) -> (k,v))) $ map snd sites

testPaths :: FilePath
          -> IO ()
testPaths src =
  do
    sites <- readLocalSites src
    mapM_ printTree $ map (\(l,t) -> mapValues (relativePath l) t) 
       $ map (\(l,t) -> (l,mapKeys nicename $ fromSite t)) sites

--testTitles :: FilePath
--          -> IO ()
--testTitles src =
--  do
--    sites <- readLocalSites src
--    mapM_ printTree $ map (fmap pageTitle) $ map fromSite sites

--testLogicalPath :: FilePath
--                -> IO ()
--testLogicalPath src =
--  do
--    sites <- readLocalSites src
--    mapM_ printTree $ map (fmap logicalPath) $ map fromSite sites

--testMenu :: FilePath
--         -> IO ()
--testMenu src =
--  do
--    sites <- readLocalSites src
--    print $ map (fmap menu) $ map fromSite sites