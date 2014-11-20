module HomepageGen.Test where

import HomepageGen.IO               (readLocalSites)
import HomepageGen.Data.Site        (localizes)
import HomepageGen.Data.Navigation  (fromSite,
                                     relativePath,
                                     pageTitle,
                                     logicalPath,
                                     Navigation)
import Data.Tree.Zipper             (TreePos,
                                     Full(..),
                                     fromTree,
                                     firstChild,
                                     next)

testPaths :: FilePath
          -> IO [FilePath]
testPaths src =
  do
    sites <- readLocalSites src
    return $ map relativePath $ concatMap fromSite sites

testTitles :: FilePath
          -> IO [String]
testTitles src =
  do
    sites <- readLocalSites src
    return $ map pageTitle $ concatMap fromSite sites

testLogicalPath :: FilePath
                -> IO [[String]]
testLogicalPath src =
  do
    sites <- readLocalSites src
    return $ map logicalPath $ concatMap fromSite sites