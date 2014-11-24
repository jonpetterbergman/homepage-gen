module Data.NavTree where

import           Data.List               (intercalate,
                                          transpose)
import           Data.List.NonEmpty      (NonEmpty(..),toList)
import qualified Data.List.NonEmpty as    NE
import           Data.Maybe              (fromMaybe)
import           Data.Either.Combinators (mapBoth)

data NavTree a b =
  Node {
  	 key        :: a
       , value      :: Either (NavTree a b) b
       , _subForest :: NavForest a b
       }
       deriving Show

type NavForest a b = [NavTree a b]

subForest :: NavTree a b
	  -> NavForest a b
subForest t = (either return (const []) $ value t) ++ (_subForest t) 

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

pad :: a
    -> [[a]]
    -> [[a]]
pad c xs = map (padn (maximum $ map length xs)) xs
  where padn n x = take n $ x ++ (repeat c)

drawTree :: NavTree String b
         -> String
drawTree = intercalate "\n" . map concat . transpose . map (pad ' ') . transpose . pad "" . toList . go
  where marry (h :| t) (h' :| t') = (h ++ h') :| ((map indent t') ++ t)
        indent xs = [""] ++ xs
        go node = 
          let xs = concatMap (toList . go) $ _subForest node in
          case value node of
            Left node' ->
              marry ([key node] :| (map indent xs)) (go node')
            Right _    ->
              [key node] :| (map indent xs)

        
ppTree :: NavTree String b
       -> IO ()
ppTree = putStrLn . drawTree