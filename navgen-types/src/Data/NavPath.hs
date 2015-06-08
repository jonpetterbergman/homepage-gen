{-# LANGUAGE FlexibleInstances #-}

module Data.NavPath where

import Data.List  (intercalate,
                   unfoldr)
import Data.Maybe (fromMaybe)

data Absolute k = Absolute [k]

data Relative k = Relative Int [k]

type NavPath k = Either (Absolute k) (Relative k)

instance Functor Absolute where
  fmap f (Absolute xs) = Absolute $ map f xs

instance Functor Relative where
  fmap f (Relative n xs) = Relative n $ map f xs

instance Show (Absolute String) where
  show (Absolute xs) = "/" ++ (intercalate "/" xs)

instance Show (Relative String) where
  show (Relative ups xs) = intercalate "/" $ (replicate ups "..") ++ xs

t1 :: Relative String
t1 = Relative 2 ["kek","lel"]

t2 :: Absolute String
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
          -> Relative String
normalize [] = Relative 0 []
normalize (Just _:Nothing:xs) = normalize xs
normalize (Just x:xs) = let Relative n xs' = normalize xs in Relative n (x:xs')
normalize (Nothing:xs) = let Relative n xs' = normalize xs in Relative (n+1) xs'    

toRelative :: Eq k
           => Absolute k
           -> Absolute k
           -> Relative k
toRelative (Absolute a) (Absolute b) = go a b
  where go [] p = Relative 0 p
        go a b | a == b = Relative 0 (take 1 $ reverse b)
        go (h:t) (h':t') | h /= h'   = Relative (length t) (h':t')
                         | otherwise = go t t'   

readPath :: String
         -> Maybe (NavPath String)
readPath ('/':xs) = 
  case normalize $ readPathElements xs of
    Relative 0 xs -> Just $ Left $ Absolute xs
    _ -> Nothing
readPath xs = Just $ Right $ normalize $ readPathElements xs