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

data Level p k a r =
  Level {
          before :: [NavTree k a r]
        , here   :: p
        , after  :: [NavTree k a r]
        } 
        deriving (Show,Eq)

type IndUpLevel k a r = Level (k,Maybe a) k a r

type UpLevel k a r = Level (k,a) k a r

type ResLevel k a r = Level r k a r

type IndLevel k a r = Level (NavNode k a r) k a r

data ResZip k a r =
  ResZip {
           res_above :: ([UpLevel k a r],[IndUpLevel k a r])
         , res_level :: ResLevel k a r
         } deriving (Show,Eq)

data IndZip k a r =
  IndZip {
           ind_above :: [IndUpLevel k a r]
         , ind_level :: IndLevel k a r
         } deriving (Show,Eq)

type NavZip k a r = Either (ResZip k a r) (IndZip k a r)

type NavLevel k a r = Either (ResLevel k a r) (IndLevel k a r)

fromTree :: NavTree k a r
         -> NavZip k a r
fromTree (Left t) = Left $ ResZip ([],[]) $ Level [] t []
fromTree (Right n) = Right $ IndZip [] $ Level [] n []

levelLeft :: NavLevel k a r
          -> Maybe (NavLevel k a r)
levelLeft (Left (Level [] _ _)) = Nothing
levelLeft (Left (Level (h:t) x r)) = Just $ mkLevel t h ((Left x):r)
levelLeft (Right (Level [] _ _))    = Nothing
levelLeft (Right (Level (h:t) x r)) = Just $ mkLevel t h ((Right x):r)

levelRight :: NavLevel k a r
           -> Maybe (NavLevel k a r)
levelRight (Left (Level _ _ [])) = Nothing
levelRight (Left (Level l x (h:t))) = Just $ mkLevel ((Left x):l) h t
levelRight (Right (Level _ _ [])) = Nothing
levelRight (Right (Level l x (h:t))) = Just $ mkLevel ((Right x):l) h t

mkLevel :: [NavTree k a r]
        -> NavTree k a r
        -> [NavTree k a r]
        -> NavLevel k a r
mkLevel before (Left r) after = Left $ Level before r after
mkLevel before (Right r) after = Right $ Level before r after


--left :: NavZip k a r
--     -> Maybe (NavZip k a r)
--left (NavZip a lev) = fmap (NavZip a) $ levelLeft lev

--right :: NavZip k a r
--      -> Maybe (NavZip k a r)
--right (NavZip a lev) = fmap (NavZip a) $ levelRight lev


--levelLeftMost :: NavLevel k a r
--              -> NavLevel k a r
--levelLeftMost lev = maybe lev levelLeftMost $ levelLeft lev

--levelRightMost :: NavLevel k a r
--               -> NavLevel k a r
--levelRightMost lev = maybe lev levelRightMost $ levelRight lev

--dup :: a 
--    -> (a,a)
--dup x = (x,x)

--levelRights :: NavLevel k a r
--            -> [NavLevel k a r]
--levelRights = unfoldr (fmap dup . levelRight)

--levelLefts :: NavLevel k a r
--           -> [NavLevel k a r]
--levelLefts = unfoldr (fmap dup . levelLeft)

--rights :: NavZip k a r
--       -> [NavZip k a r]
--rights (NavZip a lev) = map (NavZip a) $ levelRights lev

--lefts :: NavZip k a r
--       -> [NavZip k a r]
--lefts (NavZip a lev) = map (NavZip a) $ levelLefts lev

--levelAll :: NavLevel k a r
--         -> [NavLevel k a r]
--levelAll nav = 
--  let start = levelLeftMost nav in
--  start:(levelRights start)

--allOnLevel :: NavZip k a r
--           -> [NavZip k a r]
--allOnLevel (NavZip a lev) = map (NavZip a) $ levelAll lev

--isTop :: NavZip k a r
--      -> Bool
--isTop (NavZip a lev) = null a

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
