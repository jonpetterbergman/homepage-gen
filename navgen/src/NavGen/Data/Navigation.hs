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
                                         before,
                                         after,
                                         ancestors,
                                         lefts,
                                         rights)
import           NavGen.Data.Site       (Label(..),
                                         Lang,
                                         LocalLabel,
                                         LocalContent,
                                         LocalSite)
import           System.FilePath        (joinPath)

type Navigation = NavZip LocalLabel LocalContent

fromSite :: LocalSite 
         -> NavTree LocalLabel Navigation
fromSite = everything . fromTree

relativePath :: Lang
             -> Navigation
             -> FilePath
relativePath lang nav = 
  joinPath $ (map pathElement $ reverse $ ancestors nav) ++
             (mkFilename nav)
  where pathElement                                  = urlname . key . here . level
        langExt                                      = "." ++ (map toLower $ show lang)
        mkFilename nav | (isLeaf $ here $ level nav) = [(pathElement nav) ++ ".html" ++ langExt]
                       | otherwise                   = [pathElement nav,"index.html" ++ langExt]


pageTitle :: Navigation
          -> String
pageTitle = nicename . key . here . level

logicalPath :: Lang
            -> Navigation
            -> ([(String,FilePath)],String)
logicalPath lang nav =
  (map (\n -> (pageTitle n,relativePath lang n)) $ (reverse $ ancestors nav),pageTitle nav)

menu :: Lang
     -> Navigation
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
               
allPages :: [(Lang,LocalSite)]
         -> [(Lang,[Lang],Navigation)]
allPages sites = 
  let langs    = map fst sites 
      navTrees = map (\(lang,site) -> (lang,fromSite site)) sites in
 concatMap (\(lang,navTree) -> map (lang,delete lang langs,) $ map snd $ toList navTree) navTrees