module HomepageGen.Data.Navigation where

import Data.List              (unfoldr)
import Data.Maybe             (fromMaybe)
import Data.Tree              (Tree(..),
                               Forest)
import Data.Tree.Zipper       (TreePos,
                               Full(..),
                               fromTree,
                               firstChild,
                               parent,
                               parents,
                               next,
                               label,
                               after,
                               before)
import HomepageGen.Data.Site  (LocalSite,
                               LocalPage,
                               LocalContent(..),
                               urlname,
                               content,
                               contentLanguage)
import System.FilePath        (joinPath)
import Text.Pandoc.Definition (docTitle,
                               Pandoc(..),
                               Inline(..))
import Text.Pandoc.Shared     (stringify)

type Navigation = TreePos Full LocalPage

fromSite :: LocalSite 
         -> Tree Navigation
fromSite tree = go $ fromTree tree 
  where go root =  
          Node root $ descendants root

dup :: a 
    -> (a,a)
dup x = (x,x)

allChildren :: Navigation 
            -> [Navigation]
allChildren node = fromMaybe [] $
  do
     first <- firstChild node
     return $ first:(unfoldr (fmap dup . next) first)
     
descendants :: Navigation
            -> Forest Navigation
descendants = map go . allChildren
  where go node = Node node (map go $ allChildren node)

ancestors :: Navigation
          -> [Navigation]
ancestors = unfoldr (fmap dup . parent)

relativePath :: Navigation
             -> FilePath
relativePath nav = 
  joinPath $ (map pathElement $ reverse $ ancestors nav) ++
             (mkFilename nav)
  where pathElement                             = urlname . label
        langExt                                 = "." ++ (contentLanguage $ content $ label nav)
        mkFilename nav | null (allChildren nav) = [(pathElement nav) ++ ".html" ++ langExt]
                       | otherwise              = [pathElement nav,"index.html" ++ langExt]

pandocTitle :: Pandoc
            -> [Inline]
pandocTitle (Pandoc meta _) = docTitle meta

pageTitle :: Navigation
          -> String
pageTitle nav = 
  case stringify $ pandocTitle $ localContent $ content $ label nav of
    "" -> urlname $ label nav
    xs -> xs

logicalPath :: Navigation
            -> ([(String,FilePath)],String)
logicalPath nav =
  (map (\n -> (pageTitle n,relativePath n)) $ (reverse $ ancestors nav),pageTitle nav)

--menu :: Navigation
--     -> Forest (String,Maybe String)
--menu nav = moveUp (parent nav) $ (mkEnts $ before nav) ++
--                                 [Node (pageTitle nav,Nothing) []] ++
--                                 (mkEnts $ after nav)
--  where mkEnt nav = Node (pageTitle nav, Just $ relativePath nav) []
--        mkEnts = map mkEnt . map fromTree 
--        moveUp Nothing prevForest = prevForest
--        moveUp (Just nav) prevForest = moveUp (parent nav) $ (mkEnts $ before nav) ++
--                                                             [Node (mkEnt nav) prevForest] ++
--                                                             (mkEnts $ after nav)
               