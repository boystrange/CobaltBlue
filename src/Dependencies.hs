module Dependencies where

import qualified Data.Set as S
import qualified Data.Partition as P

type DependencyRelation a = P.Partition a

empty :: P.Partition a
empty = P.empty

fromList :: Ord a => [a] -> P.Partition a
fromList xs = fromSet (S.fromList xs)

fromSet :: Ord a => S.Set a -> DependencyRelation a
fromSet xset = P.fromSets [xset]

union :: Ord a => P.Partition a -> P.Partition a -> P.Partition a
union d₁ d₂ = P.fromSets $ P.nontrivialSets d₁ ++ P.nontrivialSets d₂

delete :: Ord a => a -> DependencyRelation a -> DependencyRelation a
delete x = P.fromDisjointSets . map (S.delete x) . P.nontrivialSets

edges :: P.Partition a -> [(a, a)]
edges = concat . map ((\xs -> zip xs (tail xs)) . S.toList) . P.nontrivialSets

disjointUnion :: Ord a => P.Partition a -> P.Partition a -> Either [a] (P.Partition a)
disjointUnion d₁ d₂ = foldr aux (Right d₂) (edges d₁)
  where
    aux _ (Left xs) = Left xs
    aux (u, v) (Right d) | P.find d u == P.find d v = Left [u, v]
                         | otherwise = Right (union d (fromList [u, v]))

