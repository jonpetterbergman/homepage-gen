{-# LANGUAGE FlexibleInstances #-}

module Data.NavZip where

import           Data.List        (unfoldr,
                                   null,
                                   find,
                                   intercalate)
import           Data.NavTree     (NavTree,
                                   NavNode(..),
                                   NavForest,   
                                   drawTree,
                                   test,
                                   isLeaf)
import           Data.NavPath     (NavPath(..))
import           Prelude hiding   (lookup)


data PLevel k a r = 
  PLevel {
           point_to :: NavNode k a r
         , p_before :: [NavTree k a r]
         , p_after :: [NavTree k a r]
         } deriving (Show,Eq)

data NPLevel p k a r =
  NPLevel {
            np_before :: [NavTree k a r]
          , np_here   :: p
          , np_after  :: [NavTree k a r]
          }  deriving (Show,Eq)

type IndUpLevel k a r = NPLevel (k,Maybe a) k a r

type UpLevel k a r = NPLevel (k,a) k a r

data NIndZip k a r =
  NIndZip {
            res_above :: ([UpLevel k a r],[IndUpLevel k a r])
          , res_level :: NPLevel (NavTree k a r) k a r
          } deriving (Show,Eq)

data IndZip k a r =
  IndZip {
           ind_above :: [IndUpLevel k a r]
         , ind_level :: PLevel k a r
         } deriving (Show,Eq)

type NavZip k a r = Either (IndZip k a r) (NIndZip k a r)

--type NavLevel k a r = Either (NPLevel (NavTree k a r) k a r) (PLevel k a r)

--level :: NavZip k a r -> NavLevel k a r
--level (Left r) = Left $ res_level r
--level (Right i) = Right $ ind_level i

fromTree :: NavTree k a r
         -> NavZip k a r
fromTree (Left t) = Right $ NIndZip ([],[]) $ NPLevel [] (Left t) []
fromTree (Right x) = Right $ NIndZip ([],[]) $ NPLevel [] (Right x) []  

class Level a where
  levelLeft :: a -> Maybe a
  levelRight :: a -> Maybe a

instance Level (NPLevel (NavTree k a r) k a r) where
  levelLeft (NPLevel []  _ _) = Nothing
  levelLeft (NPLevel (h:t) x aft) = Just $ NPLevel t h (x:aft)
  levelRight (NPLevel _ _ []) = Nothing
  levelRight (NPLevel bef x (h:t)) = Just $ NPLevel (x:bef) h t 
 
instance Level (PLevel k a r) where
  levelLeft (PLevel _ [] _) = Nothing
  levelLeft (PLevel pt (h:t) aft) = Just $ PLevel pt t (h:aft)
  levelRight (PLevel _ _ []) = Nothing
  levelRight (PLevel pt bef (h:t)) = Just $ PLevel pt (h:bef) t

left :: NavZip k a r
     -> Maybe (NavZip k a r)
left (Left (IndZip ab lev)) = fmap (Left . IndZip ab) $ levelLeft lev 
left (Right (NIndZip ab lev)) = fmap (Right . NIndZip ab) $ levelLeft lev

right :: NavZip k a r
      -> Maybe (NavZip k a r)
right (Left (IndZip ab lev)) = fmap (Left . IndZip ab) $ levelRight lev 
right (Right (NIndZip ab lev)) = fmap (Right . NIndZip ab) $ levelRight lev


levelLeftMost :: Level a
              => a -> a
levelLeftMost lev = maybe lev levelLeftMost $ levelLeft lev

levelRightMost :: Level a
               => a -> a
levelRightMost lev = maybe lev levelRightMost $ levelRight lev

dup :: a 
    -> (a,a)
dup x = (x,x)

levelRights :: Level a
            => a
            -> [a]
levelRights = unfoldr (fmap dup . levelRight)

levelLefts :: Level a
           => a
           -> [a]
levelLefts = unfoldr (fmap dup . levelLeft)

lefts :: NavZip k a r
       -> [NavZip k a r]
lefts (Left (IndZip a lev)) = map (Left . IndZip a) $ levelLefts lev
lefts (Right (NIndZip a lev)) = map (Right . NIndZip a) $ levelLefts lev

rights :: NavZip k a r
       -> [NavZip k a r]
rights (Left (IndZip a lev)) = map (Left . IndZip a) $ levelRights lev
rights (Right (NIndZip a lev)) = map (Right . NIndZip a) $ levelRights lev


levelAll :: Level a
         => a
         -> [a]
levelAll nav = 
  let start = levelLeftMost nav in
  start:(levelRights start)

allOnLevel :: NavZip k a r
           -> [NavZip k a r]
allOnLevel (Left (IndZip a lev)) = map (Left . IndZip a) $ levelAll lev
allOnLevel (Right (NIndZip a lev)) = map (Right . NIndZip a) $ levelAll lev

isTop :: NavZip k a r
      -> Bool
isTop (Left (IndZip a _)) = null a
isTop (Right (NIndZip (a,_) _)) = null a

--everything :: NavZip k a r
--           -> NavTree k (NavZip k a r) (NavZip k a r)
--everything z@(NavZip t (Level b (Left  x) a)) = Left z
--everything z@(NavZip t (Level b (Right n) a)) = Right $ go n
--  where go (Node k v frst) = Node k v' frst'
--          where (v',frst') =
--                  case (v,frst) of
--                    (Left n,_) -> 
--                      let first = NavZip ((Level b (k,Nothing) a):t) $ Level [] (Right n) frst in
--                      (Left $ everything first,map everything $ rights first)
--                    (Right rv,(x:xs)) -> 
--                      let first = NavZip ((Level b (k,Just rv) a):t) $ Level [] x xs in
--                      (Right z,map go $ allOnLevel first)
--                    (Right _,[]) ->
--                      (Right z,[])


up :: NavZip k a r
   -> Maybe (NavZip k a r)
up (Left (IndZip [] _)) = Nothing
up (Left (IndZip (h:t) l)) = Nothing

--up :: NavZip k a r
--   -> Maybe (NavZip k a r)
--up (NavZip [] _) = Nothing
--up (NavZip ((Level b (key,val) a):t) l) =
--  let Level _ this xs = levelLeftMost l in
--  case val of
--    Nothing   -> 
--      case this of
--        Left _ -> Nothing
--        Right this' -> Just $ NavZip t $ Level b (Right $ Node key (Left this') xs) a
--    Just val' -> Just $ NavZip t $ Level b (Right $ Node key (Right val') (this:xs)) a

--ancestors :: NavZip k a r
--          -> [NavZip k a r]
--ancestors = unfoldr (fmap dup . up)

--top :: NavZip k a r
--    -> NavZip k a r
--top nav = maybe nav top $ up nav

--lookup :: Eq k
--       => NavZip k a r
--       -> NavPath k
--       -> Maybe (NavZip k a r)
--lookup z (Absolute xs) = lookup (top z) $ Relative 0 xs
--lookup z (Relative 0 []) = Just z
--lookup z (Relative 0 (x:xs)) = 
--  do
--    z' <- find ((== x) . key . here . level) $ children z
--    lookup z' $ Relative 0 xs
--lookup z (Relative n xs) = 
--  do
--    z' <- up z
--    lookup z' $ Relative (n-1) xs

--mkPath :: (Eq k,Eq a) 
--       => NavZip k a r
--       -> NavZip k a r
--       -> Maybe (NavPath k)
--mkPath = go (0,[])
--  where go (n,xs) from to | from == to = Just $ Relative n xs
--                          | otherwise = do from' <- up from
--                                           to'   <- up to
--                                           go (n+1,(key $ here $ level to):xs) from' to'

--firstChildOrValue :: NavZip k a r
--                  -> Either a (NavZip k a r)
--firstChildOrValue (NavZip t (Level b (Node key val frst) a)) =
--  case (val,frst) of 
--    (Left n,xs) ->
--      Right $ NavZip ((Level b (key,Nothing) a):t) (Level [] n xs)
--    (Right val',(x:xs)) ->
--      Right $ NavZip ((Level b (key,Just val') a):t) (Level [] x xs)
--    (Right val',_) ->
--      Left val'

--firstChild :: NavZip k a r
--           -> Maybe (NavZip k a r)
--firstChild = either (const Nothing) Just . firstChildOrValue

--children :: NavZip k a r
--         -> [NavZip k a r]
--children = maybe [] allOnLevel . firstChild

--followLink :: NavZip k a r
--           -> NavZip k a r
--followLink nav@(NavZip t (Level b (Node key val frst) a)) =
--  case val of
--    (Left _)  -> maybe nav followLink $ firstChild nav
--    (Right _) -> nav 

--adjustKey :: (k -> k)
--          -> NavZip k a r
--          -> NavZip k a r
--adjustKey f (NavZip t (Level b (Node k v frst) a)) = 
--  NavZip t (Level b (Node (f k) v frst) a)

--drawZip :: NavZip String String String
--        -> String
--drawZip = drawTree . here . level . top . adjustKey (++ "<-")

--ppZip :: NavZip String String  String
--      -> IO ()
--ppZip = putStrLn . drawZip

--ppMZip :: Maybe (NavZip String String String)
--       -> IO ()
--ppMZip = maybe (putStrLn "NOTHING") ppZip

--showRelativePath :: NavZip a b r
--                 -> NavPath String
--                 -> String
--showRelativePath nav (Absolute xs) = "/" ++ (intercalate "/" xs)
--showRelativePath nav (Relative ups xs) = 
--  intercalate "/" $ replicate n ".." ++ xs
--  where n = if (isLeaf $ here $ level nav) then ups - 1 else ups

--makeRelativePath :: Eq a 
--                 => [a]
--                 -> NavZip a b r
--                 -> Maybe (NavPath a)
--makeRelativePath abs nav = go 0 abs nav
--  where go n [] nav = maybe (Just $ Relative n []) (go (n+1) []) $ up nav
--        go n abs@(h:t) nav = 
--          case find ((== h) . key . here . level) $ allOnLevel nav of
--            Just _ -> Just $ Relative n abs
--            Nothing -> up nav >>= go (n+1) abs
