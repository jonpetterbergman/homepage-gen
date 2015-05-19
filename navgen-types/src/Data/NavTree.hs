module Data.NavTree where

import           Data.List               (intercalate,
                                          transpose)
import           Data.List.NonEmpty      (NonEmpty(..))
import qualified Data.List.NonEmpty as    NE
import           Data.Maybe              (fromMaybe)
import           Data.Either.Combinators (mapBoth)

data NavTree a b =
  Node {
         key        :: a
       , value      :: Either (NavTree a b) b
       , _subForest :: NavForest a b
       }
       deriving (Show,Eq)

followValue :: NavTree a b
            -> b
followValue n =
  case value n of
    Left n' -> followValue n'
    Right x -> x

followKeys :: NavTree a b
           -> [a]
followKeys n =
  case value n of
    Left n' -> key n:followKeys n'
    Right x -> [key n]

type NavForest a b = [NavTree a b]

subForest :: NavTree a b
          -> NavForest a b
subForest t = (either return (const []) $ value t) ++ (_subForest t) 

isLeaf :: NavTree a b
       -> Bool
isLeaf (Node _ (Right _) []) = True
isLeaf _                     = False

test = Node "petter" (Right ()) [photo,axis,deep,haskell]
  where photo    = Node "photo"  (Right ())    [photo1,photo2,photo3]
        photo1   = Node "photo1" (Right ())    []
        photo2   = Node "photo2" (Right ())    []
        photo3   = Node "photo3" (Right ())    []
        axis     = Node "axis"   (Left axisix) [axis2]
        axisix   = Node "axisix" (Right ())    []
        axis2    = Node "axis2"  (Right ())    []
        deep     = Node "deep"   (Left deepix) [deep1,deep2]
        deep1    = Node "deep1"  (Right ()) []
        deep2    = Node "deep2"  (Right ()) []
        deepix   = Node "deepix" (Left deeperix) [deeper1,deeper2]
        deeper1  = Node "deeper1" (Right ()) []
        deeper2  = Node "deeper2" (Right ()) []
        deeperix = Node "deeperix" (Right ()) []
        haskell  = Node "haskell" (Right ())   [haskell1,haskell2]
        haskell1 = Node "haskell1" (Right ()) []
        haskell2 = Node "haskell2" (Right ()) [] 

mapValues :: (v -> v')
          -> NavTree k v
          -> NavTree k v'
mapValues f (Node k v sf) =
  Node k (either (Left . mapValues f) (Right . f) v) $ map (mapValues f) sf

mapKeys :: (k -> k') 
        -> NavTree k v
        -> NavTree k' v
mapKeys f (Node k v sf) = 
  Node (f k) (either (Left . mapKeys f) Right v) $ map (mapKeys f) sf

mapWithKeys :: (k -> k')
            -> (v -> v')
            -> NavTree k v
            -> NavTree k' v'
mapWithKeys f g (Node k v sf) =
  let k' = f k 
      v' = mapBoth (mapWithKeys f g) g v in
  Node k' v' $ map (mapWithKeys f g) sf

toList :: NavTree k v
       -> [(k,v)]
toList (Node _ (Left  n) ns) = toList n ++ (concatMap toList ns)
toList (Node k (Right v) ns) = (k,v):(concatMap toList ns)

pad :: a
    -> [[a]]
    -> [[a]]
pad c xs = map (padn (maximum $ map length xs)) xs
  where padn n x = take n $ x ++ (repeat c)

drawTree :: NavTree String String
         -> String
drawTree nav = 
  let (keytbl,vals) = unzip $ NE.toList $ go nav in
  intercalate "\n" $ zipWith (\v k -> k ++ " " ++ (fromMaybe "-" v)) vals $ map concat $ transpose $ map (pad ' ') $ transpose $ pad "" $ keytbl
  where marry ((h,_) :| t) ((h',x) :| t') = (h ++ h',x) :| ((map indent t') ++ t)
        indent (xs,x) = ([""] ++ xs,x)
        go node = 
          let xs = concatMap (NE.toList . go) $ _subForest node in
          case value node of
            Left node' ->
              marry (([key node],Nothing) :| (map indent xs)) (go node')
            Right x    ->
              ([key node],Just x) :| (map indent xs)

        
ppTree :: NavTree String String
       -> IO ()
ppTree = putStrLn . drawTree
