{-# LANGUAGE TupleSections #-}
module HomepageGen.Data.Site where

import           Data.Function        (on)
import           Data.List            (groupBy,
                                       sortBy)
import           Data.Map             (Map)
import qualified Data.Map          as  Map
import           Data.Monoid          (Monoid(..))
import           Data.Text            (Text)
import qualified Data.Text.Lazy    as  LT
import           Data.Tree            (Tree(..),
                                       Forest)

type Lang = String

data Page =
  Page {
         urlname  :: FilePath
       , nicename :: Map Lang Text
       , content  :: Map Lang LT.Text
       } 
       deriving Show      

instance Monoid Page where
  mempty = Page "" mempty mempty
  mappend p1 p2 = Page (urlname p1) 
                       (Map.union (nicename p1) (nicename p2))
                       (Map.union (content p1) (content p2))

type Site = Tree Page

splitLeaves :: Forest a
            -> ([a],Forest a)
splitLeaves = foldr go ([],[])
  where go (Node x []) (leaves,trees) = (x:leaves,trees)
        go tree        (leaves,trees) = (leaves,tree:trees)

joinPages :: Forest Page
          -> Forest Page
joinPages pgs =
  let (leaves,trees) = splitLeaves pgs in
  (map (flip Node $ []) $ map mconcat $ groupBy ((==) `on` urlname) 
                                      $ sortBy (compare `on` urlname) leaves) 
  ++ trees


