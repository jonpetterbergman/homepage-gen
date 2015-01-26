{-# LANGUAGE TupleSections,
             TypeSynonymInstances,
             FlexibleInstances #-}
module NavGen.Data.Site where

import           Control.Monad        (join)
import           Data.Function        (on)
import           Data.LanguageCodes   (ISO639_1)
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

type Lang = ISO639_1                     

data Label a =
  Label {
          urlname  :: String
        , nicename :: a
        }
        deriving Show

type IntlLabel = Label (Map Lang String)

type IntlContent a = Map Lang a

type LocalLabel = Label String

type IntlSite a = NavTree IntlLabel (IntlContent a)

type LocalSite a = NavTree LocalLabel a

splitLeaves :: NavForest k a
            -> ([(k,a)],NavForest k a)
splitLeaves = foldr go ([],[])
  where go (Node k (Right v) []) (leaves,trees) = ((k,v):leaves,trees)
        go (Node k _         []) (leaves,trees) = (leaves,trees)
        go tree                  (leaves,trees) = (leaves,tree:trees)

localize :: IntlSite a
         -> (Lang,String,a)
         -> LocalSite a
localize site (lang,defaultNiceName,defaultPage) = mapWithKeys localK localC site
  where localK (Label url mp) = Label url $ fromMaybe defaultNiceName $ Map.lookup lang mp
        localC mp = fromMaybe defaultPage $ Map.lookup lang mp 

localizes :: IntlSite a
          -> [(Lang,String,a)]
          -> [(Lang,LocalSite a)]
localizes site = map go
  where go tr@(lang,_,_) = (lang,localize site tr)


