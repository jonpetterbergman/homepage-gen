{-# LANGUAGE TupleSections,
             TypeSynonymInstances,
             FlexibleInstances #-}
module HomepageGen.Data.Site where

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
import           Text.Pandoc          (Pandoc(..),
                                       Inline,
                                       docTitle)
import           Text.Pandoc.Shared   (stringify)

type Lang = ISO639_1                     

data Label a =
  Label {
          urlname  :: String
        , nicename :: a
        }
        deriving Show

type IntlLabel = Label (Map Lang String)

type IntlContent = Map Lang Pandoc

type LocalLabel = Label String

type LocalContent = Pandoc

type IntlSite = NavTree IntlLabel IntlContent

type LocalSite = NavTree LocalLabel LocalContent

splitLeaves :: NavForest k a
            -> ([(k,a)],NavForest k a)
splitLeaves = foldr go ([],[])
  where go (Node k (Right v) []) (leaves,trees) = ((k,v):leaves,trees)
        go (Node k _         []) (leaves,trees) = (leaves,trees)
        go tree                  (leaves,trees) = (leaves,tree:trees)

--appendPages :: Bool
--appendPages = False

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
          -> [(Lang,LocalSite)]
localizes site = map go
  where go tr@(lang,_,_) = (lang,localize site tr)

pandocTitle :: Pandoc
            -> [Inline]
pandocTitle (Pandoc meta _) = docTitle meta

pageTitle :: Pandoc
          -> Maybe String
pageTitle doc = 
  case stringify $ pandocTitle doc of
    "" -> Nothing
    xs -> Just xs
