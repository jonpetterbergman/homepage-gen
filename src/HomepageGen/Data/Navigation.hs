module HomepageGen.Data.Navigation where

import Data.Char              (toLower)
import Data.List              (unfoldr)
import Data.Maybe             (fromMaybe)
import Data.NavTree           (NavTree(..),
                               NavForest,
                               isLeaf)
import Data.NavZip            (NavZip(..),
                               fromTree,
                               everything,
                               followLink,
                               here,
                               level,   
                               ancestors)
import HomepageGen.Data.Site  (Label(..),
                               Lang,
                               LocalLabel,
                               LocalContent,
                               LocalSite)
import System.FilePath        (joinPath)
import Text.Pandoc.Definition (docTitle,
                               Pandoc(..),
                               Inline(..))
import Text.Pandoc.Shared     (stringify)

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

--befores :: Navigation
--        -> [Navigation]
--befores = unfoldr (fmap dup . prev)

--afters :: Navigation
--       -> [Navigation]
--afters = unfoldr (fmap dup . prev)

--menu :: Navigation
--     -> Forest (String,Maybe String)
--menu nav = moveUp (parent nav) $ (map mkNode $ befores nav) ++
--                                 [Node (pageTitle nav,Nothing) []] ++
--                                 (map mkNode $ afters nav)
--  where mkEnt nav = (pageTitle nav, Just $ relativePath nav)
--        mkNode nav = Node (mkEnt nav) []
--        moveUp Nothing prevForest = prevForest
--        moveUp (Just nav) prevForest = moveUp (parent nav) $ (map mkNode $ befores nav) ++
--                                                             [Node (mkEnt nav) prevForest] ++
--                                                             (map mkNode $ afters nav)
               