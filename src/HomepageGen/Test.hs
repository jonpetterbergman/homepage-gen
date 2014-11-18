module HomepageGen.Test where

import HomepageGen.IO(readSite)
import HomepageGen.Data.Navigation(fromSite)
import Data.Tree.Zipper      (TreePos,
                              Full(..),
                              fromTree,
                              firstChild,
                              next)

