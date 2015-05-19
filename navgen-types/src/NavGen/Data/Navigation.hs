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
import           Data.NavPath           (NavPath(..))
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

pageUrl :: Navigation a
        -> String
pageUrl = urlname . key . here . level

logicalPath :: Navigation a
             -> ([(String,NavPath String)],String)
logicalPath nav = (unfoldr go $ (0,nav),pageTitle nav)
  where go (n,nav') = 
         case up nav' of
           Nothing   -> Nothing
           Just nav'' -> Just ((pageTitle nav'',Relative (n+1) []),(n+1,nav''))


menu :: Navigation a
     -> Forest (String,Maybe (NavPath String),Bool)
menu nav = moveUp (up nav,1) $ (map (mkNode 0) $ reverse $ lefts nav) ++
                                [Tree.Node (pageTitle nav,Nothing,isLeaf $ here $ level nav) []] ++
                                (map (mkNode 0) $ rights nav)
  where mkEnt n nav = (pageTitle nav, Just $ Relative n [pageUrl nav],isLeaf $ here $ level nav)
        mkNode n nav = Tree.Node (mkEnt n nav) []
        moveUp (Nothing,_) prevForest = prevForest
        moveUp (Just nav,n) prevForest = moveUp (up nav,n+1) $ (map (mkNode n) $ reverse $ lefts nav) ++
                                                                [Tree.Node (mkEnt n nav) prevForest] ++
                                                                (map (mkNode n) $ rights nav)
               
allPages :: [(Lang,LocalSite a)]
         -> [(Lang,[Lang],Navigation a)]
allPages sites = 
  let langs    = map fst sites 
      navTrees = map (\(lang,site) -> (lang,fromSite site)) sites in
 concatMap (\(lang,navTree) -> map (lang,delete lang langs,) $ map snd $ toList navTree) navTrees