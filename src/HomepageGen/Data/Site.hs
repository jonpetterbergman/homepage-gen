{-# LANGUAGE TupleSections,
             TypeSynonymInstances,
             FlexibleInstances #-}
module HomepageGen.Data.Site where

import           Control.Monad        (join)
import           Data.Function        (on)
import           Data.List            (groupBy,
                                       sortBy)
import           Data.Map             (Map)
import qualified Data.Map          as  Map
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          (Monoid(..))
import           Data.Text            (Text)
import qualified Data.Text.Lazy    as  LT
import qualified Data.Traversable  as  Tr
import           Data.Tree            (Tree(..),
                                       Forest)
import           Text.Pandoc          (Pandoc)

type Lang = String                     

data Page a =
  Page {
         urlname  :: FilePath
       , content  :: a
       }          
       deriving Show      

type IntlPage = Page (Map Lang Pandoc)

data LocalContent =
  LocalContent {
                 contentLanguage :: Lang 
               , localContent :: Pandoc
               }

type LocalPage = Page LocalContent

appendPages :: IntlPage 
            -> IntlPage
            -> IntlPage
appendPages p1 p2 = Page (urlname p1) 
                         (Map.union (content p1) (content p2))

type IntlSite = Tree IntlPage

type LocalSite = Tree LocalPage

splitLeaves :: Forest a
            -> ([a],Forest a)
splitLeaves = foldr go ([],[])
  where go (Node x []) (leaves,trees) = (x:leaves,trees)
        go tree        (leaves,trees) = (leaves,tree:trees)

joinPages :: Forest IntlPage
          -> Forest IntlPage
joinPages pgs =
  let (leaves,trees) = splitLeaves pgs in
  (map (flip Node $ []) $ Map.elems $ 
                          foldr (\l -> Map.insertWith appendPages 
                                                      (urlname l) l) 
                                mempty leaves)  
  ++ trees

localize :: IntlSite
         -> LocalContent
         -> LocalSite
localize site defaultPage = fmap go site
  where go (Page n mp) = 
          let lang = contentLanguage defaultPage in
          fromMaybe (Page n $ defaultPage) $ fmap (Page n . LocalContent lang) $ Map.lookup lang mp

localizes :: IntlSite 
          -> [LocalContent]
          -> [LocalSite]
localizes site = map $ localize site
