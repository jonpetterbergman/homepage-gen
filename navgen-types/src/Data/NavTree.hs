module Data.NavTree where

import           Data.List               (intercalate,
                                          transpose)
import           Data.List.NonEmpty      (NonEmpty(..))
import qualified Data.List.NonEmpty as    NE
import           Data.Maybe              (fromMaybe)
import           Data.Either.Combinators (mapBoth)

data NavNode a b r =
  Node {
         key        :: a
       , value      :: Either (NavNode a b r) b
       , _subForest :: NavForest a b r
       }
       deriving (Show,Eq)

type NavTree a b r = Either r (NavNode a b r)

type NavForest a b r = [NavTree a b r]


followValue :: NavNode a b r
            -> b
followValue n =
  case value n of
    Left n' -> followValue n'
    Right x -> x

followKeys :: NavNode a b r
           -> [a]
followKeys n =
  case value n of
    Left n' -> key n:followKeys n'
    Right x -> [key n]


subForest :: NavNode a b r
          -> NavForest a b r
subForest t = (map Right $ either return (const []) $ value t) ++ (_subForest t) 

isLeaf :: NavTree a b r
       -> Bool
isLeaf (Right (Node _ (Right _) [])) = True
isLeaf _                     = False

test = Right $ Node "petter" (Right ()) [photo,axis,deep,haskell,res]
  where photo    = Right $ Node "photo"  (Right ())    [photo1,photo2,photo3]
        photo1   = Right $ Node "photo1" (Right ())    []
        photo2   = Right $ Node "photo2" (Right ())    []
        photo3   = Right $ Node "photo3" (Right ())    []
        axis     = Right $ Node "axis"   (Left axisix) [axis2]
        axisix   = Node "axisix" (Right ())    []
        axis2    = Right $ Node "axis2"  (Right ())    []
        deep     = Right $ Node "deep"   (Left deepix) [deep1,deep2]
        deep1    = Right $ Node "deep1"  (Right ()) []
        deep2    = Right $ Node "deep2"  (Right ()) []
        deepix   = Node "deepix" (Left deeperix) [deeper1,deeper2]
        deeper1  = Right $ Node "deeper1" (Right ()) []
        deeper2  = Right $ Node "deeper2" (Right ()) []
        deeperix = Node "deeperix" (Right ()) []
        haskell  = Right $ Node "haskell" (Right ())   [haskell1,haskell2]
        haskell1 = Right $ Node "haskell1" (Right ()) []
        haskell2 = Right $ Node "haskell2" (Right ()) [] 
        res      = Left "kek"

mapValues :: (v -> v')
          -> NavTree k v r
          -> NavTree k v' r
mapValues _ (Left x) = Left x
mapValues f (Right n) = Right $ go n
  where go (Node k v sf) =
               Node k (either (Left . go) (Right . f) v) $ map (mapValues f) sf

mapKeys :: (k -> k') 
        -> NavTree k v r
        -> NavTree k' v r
mapKeys _ (Left x) = Left x
mapKeys f (Right n) = Right $ go n
  where go (Node k v sf) = 
               Node (f k) (either (Left . go) Right v) $ map (mapKeys f) sf

mapWithKeys :: (k -> k')
            -> (v -> v')
            -> NavTree k v r
            -> NavTree k' v' r
mapWithKeys _ _ (Left x) = Left x
mapWithKeys f g (Right n) = Right $ go n
  where go (Node k v sf) = Node k' v' $ map (mapWithKeys f g) sf
          where k' = f k 
                v' = mapBoth go g v
  

toList :: NavTree k v r
       -> [Either r (k,v)]
toList (Left x) = [Left x]
toList (Right n) = go n
  where go (Node _ (Left  n) ns) = go n ++ (concatMap toList ns)
        go (Node k (Right v) ns) = (Right (k,v)):(concatMap toList ns)

pad :: a
    -> [[a]]
    -> [[a]]
pad c xs = map (padn (maximum $ map length xs)) xs
  where padn n x = take n $ x ++ (repeat c)

drawTree :: NavTree String String String
         -> String
drawTree nav = 
  let (keytbl,vals) = unzip $ NE.toList $ go nav in
  intercalate "\n" $ zipWith (\v k -> k ++ " " ++ (fromMaybe "-" v)) vals $ map concat $ transpose $ map (pad ' ') $ transpose $ pad "" $ keytbl
  where marry ((h,_) :| t) ((h',x) :| t') = (h ++ h',x) :| ((map indent t') ++ t)
        indent (xs,x) = ([""] ++ xs,x)
        go (Left x) = ([x],Just "<file>") :| []
        go (Right node) = 
          let xs = concatMap (NE.toList . go) $ _subForest node in
          case value node of
            Left node' ->
              marry (([key node],Nothing) :| (map indent xs)) (go $ Right node')
            Right x    ->
              ([key node],Just x) :| (map indent xs)

        
ppTree :: NavTree String String String
       -> IO ()
ppTree = putStrLn . drawTree
