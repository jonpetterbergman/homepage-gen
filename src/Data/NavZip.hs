module Data.NavZip where

import           Data.NavTree     (NavTree(..),
                                   NavForest,   
                                   drawTree,
                                   test)

data Level p k a =
  Level {
          before :: [NavTree k a]
        , here   :: p
        , after  :: [NavTree k a]
        } 
        deriving Show

type UpLevel k a = Level (k,Maybe a) k a

type NavLevel k a = Level (NavTree k a) k a

data NavZip k a =
  NavZip {
           above :: [UpLevel k a]
         , level :: NavLevel k a
         }
         deriving Show

fromTree :: NavTree k a
         -> NavZip k a
fromTree t = NavZip [] $ Level [] t []

levelLeft :: NavLevel k a
          -> Maybe (NavLevel k a)
levelLeft (Level [] _ _)    = Nothing
levelLeft (Level (h:t) x r) = Just $ Level t h (x:r)

levelRight :: NavLevel k a
           -> Maybe (NavLevel k a)
levelRight (Level _ _ [])    = Nothing
levelRight (Level l x (h:t)) = Just $ Level (x:l) h t

left :: NavZip k a
     -> Maybe (NavZip k a)
left (NavZip a lev) = fmap (NavZip a) $ levelLeft lev

right :: NavZip k a
      -> Maybe (NavZip k a)
right (NavZip a lev) = fmap (NavZip a) $ levelRight lev

levelLeftMost :: NavLevel k a
              -> NavLevel k a
levelLeftMost lev = maybe lev levelLeftMost $ levelLeft lev

levelRightMost :: NavLevel k a
               -> NavLevel k a
levelRightMost lev = maybe lev levelRightMost $ levelRight lev

up :: NavZip k a 
   -> Maybe (NavZip k a)
up (NavZip [] _) = Nothing
up (NavZip ((Level b (key,val) a):t) l) =
  let Level _ this xs = levelLeftMost l in
  case val of
    Nothing   -> Just $ NavZip t $ Level b (Node key (Left this) xs) a
    Just val' -> Just $ NavZip t $ Level b (Node key (Right val') (this:xs)) a

top :: NavZip k a
    -> NavZip k a
top nav = maybe nav top $ up nav

firstChild :: NavZip k a
           -> Maybe (NavZip k a)
firstChild (NavZip t (Level b (Node key val frst) a)) =
  case (val,frst) of 
    (Left n,xs) ->
      Just $ NavZip ((Level b (key,Nothing) a):t) (Level [] n xs)
    (Right val',(x:xs)) ->
      Just $ NavZip ((Level b (key,Just val') a):t) (Level [] x xs)
    _ ->
      Nothing

adjustKey :: (k -> k)
          -> NavZip k a
          -> NavZip k a
adjustKey f (NavZip t (Level b (Node k v frst) a)) = 
  NavZip t (Level b (Node (f k) v frst) a)

drawZip :: NavZip String a
        -> String
drawZip = drawTree . here . level . top . adjustKey (++ "<-")

ppZip :: NavZip String a  
      -> IO ()
ppZip = putStrLn . drawZip

ppMZip :: Maybe (NavZip String a)
       -> IO ()
ppMZip = maybe (putStrLn "NOTHING") ppZip