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
import           Data.NavTree         (NavTree(..),
                                       NavForest,
                                       mapWithKeys)
import           Text.Pandoc          (Pandoc)

type Lang = String                     

data Label a =
  Label {
          urlname  :: String
        , nicename :: a
        }

type IntlLabel = Label (Map Lang String)

type IntlContent = Map Lang Pandoc

type LocalLabel = Label String

type LocalContent = Pandoc

type IntlSite = NavTree IntlLabel IntlContent

type LocalSite = NavTree LocalLabel LocalContent

--splitLeaves :: Forest a
--            -> ([a],Forest a)
--splitLeaves = foldr go ([],[])
--  where go (Node x []) (leaves,trees) = (x:leaves,trees)
--        go tree        (leaves,trees) = (leaves,tree:trees)

--joinPages :: Forest IntlPage
--          -> Forest IntlPage
--joinPages pgs =
--  let (leaves,trees) = splitLeaves pgs in
--  (map (flip Node $ []) $ Map.elems $ 
--                          foldr (\l -> Map.insertWith appendPages 
--                                                      (urlname l) l) 
--                                mempty leaves)  
--  ++ trees

localize :: IntlSite
         -> (Lang,String,LocalContent)
         -> LocalSite
localize site (lang,defaultNiceName,defaultPage) = mapWithKeys localK localC site
  where localK (Label url mp) = Label url $ fromMaybe defaultNiceName $ Map.lookup lang mp
        localC mp = fromMaybe defaultPage $ Map.lookup lang mp 

localizes :: IntlSite 
          -> [(Lang,String,LocalContent)]
          -> [LocalSite]
localizes site = map $ localize site