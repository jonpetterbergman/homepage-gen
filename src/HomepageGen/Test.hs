module HomepageGen.Test where

import HomepageGen.IO               (readLocalSites)
import HomepageGen.Data.Site        (localizes)
import HomepageGen.Data.Navigation  (fromSite,
                                     relativePath,
                                     pageTitle,
                                     logicalPath,
                                     Navigation,
                                     menu)
import Data.Tree                    (Tree,
                                     drawTree)
import Data.Tree.Zipper             (TreePos,
                                     Full(..),
                                     fromTree,
                                     firstChild,
                                     next)

printTree :: Show a
          => Tree a
          -> IO ()
printTree = putStrLn . drawTree . fmap show

testPaths :: FilePath
          -> IO ()
testPaths src =
  do
    sites <- readLocalSites src
    mapM_ printTree $ map (fmap relativePath) $ map fromSite sites

testTitles :: FilePath
          -> IO ()
testTitles src =
  do
    sites <- readLocalSites src
    mapM_ printTree $ map (fmap pageTitle) $ map fromSite sites

testLogicalPath :: FilePath
                -> IO ()
testLogicalPath src =
  do
    sites <- readLocalSites src
    mapM_ printTree $ map (fmap logicalPath) $ map fromSite sites

testMenu :: FilePath
         -> IO ()
testMenu src =
  do
    sites <- readLocalSites src
    print $ map (fmap menu) $ map fromSite sites