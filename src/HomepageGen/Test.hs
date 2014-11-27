module HomepageGen.Test where

import HomepageGen.IO               (readLocalSites)
import HomepageGen.Data.Site        (urlname,
                                     Label(..))
--import HomepageGen.Data.Navigation  (fromSite,
--                                     relativePath,
--                                     pageTitle,
--                                     logicalPath,
--                                     Navigation,
--                                     menu)
import Data.NavTree                 (NavTree,
                                     ppTree,
                                     mapKeys)
--import Data.Tree.Zipper             (TreePos,
--                                     Full(..),
--                                     fromTree,
--                                     firstChild,
--                                     next)

printTree :: Show k
          => NavTree k b
          -> IO ()
printTree = ppTree . mapKeys show

testUrlNames :: FilePath
             -> IO ()
testUrlNames src =
  do
    sites <- readLocalSites src
    mapM_ printTree $ map (mapKeys (\(Label k v) -> (k,v))) sites

--testPaths :: FilePath
--          -> IO ()
--testPaths src =
--  do
--    sites <- readLocalSites src
--    mapM_ printTree $ map (fmap relativePath) $ map fromSite sites

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