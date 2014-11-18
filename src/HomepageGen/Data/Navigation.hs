module HomepageGen.Data.Navigation where

import Data.List             (unfoldr)
import Data.Maybe            (fromMaybe)
import Data.Tree.Zipper      (TreePos,
                              Full(..),
                              fromTree,
                              firstChild,
                              next)
import HomepageGen.Data.Site (Site,
                              Page)
import Debug.Trace


type Navigation = TreePos Full Page

fromSite :: Site 
         -> [Navigation]
fromSite tree = go $ fromTree tree 
  where go root =  
          root:(concatMap go $ allChildren root)

dup :: Show a => a 
    -> (a,a)
dup x = trace (show x) $ (x,x)

allChildren :: Navigation -> [Navigation]
allChildren node = fromMaybe [] $
  do
     first <- firstChild node
     return $ first:(unfoldr (fmap dup . next) first)
     