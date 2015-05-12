{-# LANGUAGE FlexibleInstances #-}

module Data.NavPath where

import Data.List  (intercalate,
                   unfoldr)
import Data.Maybe (fromMaybe)

data NavPath k =
    Absolute [k]
  | Relative Int [k] deriving Eq

instance Show (NavPath String) where
  show (Absolute xs) = "/" ++ (intercalate "/" xs)
  show (Relative ups xs) = intercalate "/" $ (replicate ups "..") ++ xs


t1 :: NavPath String
t1 = Relative 2 ["kek","lel"]

t2 :: NavPath String
t2 = Absolute ["kek","lel"]

readPathElement :: String -> Maybe (Maybe String,String)
readPathElement [] = Nothing
readPathElement xs = Just $
  case break (== '/') xs of
    ([],xs)   -> (Just xs,[])
    ("..",xs) -> (Nothing,drop 1 xs)
    (x,xs)    -> (Just x,drop 1 xs)

readPathElements :: String -> [Maybe String]
readPathElements = unfoldr readPathElement

normalize :: [Maybe String]
          -> NavPath String
normalize [] = Relative 0 []
normalize (Just _:Nothing:xs) = normalize xs
normalize (Just x:xs) = let Relative n xs' = normalize xs in Relative n (x:xs')
normalize (Nothing:xs) = let Relative n xs' = normalize xs in Relative (n+1) xs'    

readPath :: String
         -> Maybe (NavPath String)
readPath ('/':xs) = 
  case normalize $ readPathElements xs of
    Relative 0 xs -> Just $ Absolute xs
    _ -> Nothing
readPath xs = Just $ normalize $ readPathElements xs