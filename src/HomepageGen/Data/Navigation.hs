module HomepageGen.Data.Navigation where

import Data.List              (unfoldr)
import Data.Maybe             (fromMaybe)
import Data.Tree              (Tree)
import Data.Tree.Zipper       (TreePos,
                               Full(..),
                               fromTree,
                               firstChild,
                               parent,
                               next,
                               label)
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
         -> [Navigation]
fromSite tree = go $ fromTree tree 
  where go root =  
          root:(descendants root)

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
            -> [Navigation]
descendants = concatMap go . allChildren
  where go node = node:(concatMap go $ allChildren node)

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

menu :: Navigation
     -> Tree (String,Maybe String)
menu = undefined