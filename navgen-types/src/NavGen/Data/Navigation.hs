{-# LANGUAGE TupleSections #-}
module NavGen.Data.Navigation where

import           Data.Char              (toLower)
import           Data.List              (unfoldr,
                                         delete)
import           Data.Maybe             (fromMaybe)
import           Data.Tree              (Tree,
                                         Forest)
import qualified Data.Tree           as Tree
import           Data.NavTree           (NavTree(..),
                                         NavForest,
                                         isLeaf,
                                         toList)
import           Data.NavZip            (NavZip(..),
                                         fromTree,
                                         everything,
                                         followLink,
                                         here,
                                         level,
                                         up,   
                                         isTop,
                                         before,
                                         after,
                                         ancestors,
                                         lefts,
                                         rights)
import           NavGen.Data.Site       (Label(..),
                                         Lang,
                                         LocalLabel,
                                         LocalSite)
import           System.FilePath        (joinPath)

type Navigation a = NavZip LocalLabel a

fromSite :: LocalSite a
         -> NavTree LocalLabel (Navigation a)
fromSite = everything . fromTree

relativePath :: Lang
             -> Navigation a
             -> FilePath
relativePath lang nav = 
  joinPath $ (map pathElement $ reverse $ ancestors nav) ++
             (mkFilename nav)
  where pathElement                                  = urlname . key . here . level
        langExt                                      = "." ++ (map toLower $ show lang)
        mkFilename nav | (isLeaf $ here $ level nav) && not (isTop nav) = [(pathElement nav) ++ ".html" ++ langExt]
                       | otherwise                   = [pathElement nav,"index.html" ++ langExt]


pageTitle :: Navigation a
          -> String
pageTitle = nicename . key . here . level

logicalPath :: Lang
            -> Navigation a
            -> ([(String,FilePath)],String)
logicalPath lang nav =
  (map (\n -> (pageTitle n,relativePath lang n)) $ (reverse $ ancestors nav),pageTitle nav)

menu :: Lang
     -> Navigation a
     -> Forest (String,Maybe String)
menu lang nav = moveUp (up nav) $ (map mkNode $ reverse $ lefts nav) ++
                                   [Tree.Node (pageTitle nav,Nothing) []] ++
                                   (map mkNode $ rights nav)
  where mkEnt nav = (pageTitle nav, Just $ relativePath lang nav)
        mkNode nav = Tree.Node (mkEnt nav) []
        moveUp Nothing prevForest = prevForest
        moveUp (Just nav) prevForest = moveUp (up nav) $ (map mkNode $ reverse $ lefts nav) ++
                                                          [Tree.Node (mkEnt nav) prevForest] ++
                                                          (map mkNode $ rights nav)
               
allPages :: [(Lang,LocalSite a)]
         -> [(Lang,[Lang],Navigation a)]
allPages sites = 
  let langs    = map fst sites 
      navTrees = map (\(lang,site) -> (lang,fromSite site)) sites in
 concatMap (\(lang,navTree) -> map (lang,delete lang langs,) $ map snd $ toList navTree) navTrees