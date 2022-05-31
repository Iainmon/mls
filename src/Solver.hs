{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Solver where

import Lib
import Language
import Kripke
import Logic 

import Data.Set (Set)
import qualified Data.Set as Set


import Data.List (inits, tails, nub, permutations, nubBy, isPrefixOf, sort)

realWorld :: State prim
realWorld = []

findModels :: (Eq agent, Eq prim) => L agent prim -> Collection (KripkeStar agent prim)
findModels formula = filter satisfies models
  where models      = everyKSM (primsUsed formula) (agentsUsed formula) realWorld
        satisfies m = sem m realWorld formula




type PrimProp = Int
type Agent    = Int
type World    = State PrimProp

type Formula = L Agent PrimProp


data Traversal a b = Step a [(Traversal a b, b)] deriving Show

class Search g where
  options :: Eq a => g a b -> a -> [a]
  walk :: Eq a => g a b -> (a -> [b]) -> a -> Traversal a b

data Graph a b = G { nodes :: [a], edges :: [(a,a,b)] } deriving Show

instance Search Graph where
  options g a = nodes g
  walk g@(G{nodes,edges}) f a = Step a [ (walk g f a',b) | a' <- options g a, b <- f a]


satisfy agnt (Prim p) vs = if elem p vs then [agnt] else []
satisfy agnt (Neg p) vs = if null $ satisfy agnt p vs then [agnt] else [] 
satisfy agnt (And p1 p2) vs = if not $ null $ satisfy agnt p1 vs `intersect` satisfy agnt p2 vs then [agnt] else []
satisfy agnt (Know a p) vs = if not $ null $ satisfy a p vs then [agnt] else []

search ag fm = satisfy ag fm

g1 = G { nodes = subsets [1,2,3], edges = [] }

spanning :: (Eq a,Eq b) => Graph a b -> Traversal a b -> Graph a b
spanning g (Step a []) = g
spanning g (Step a tvs) = G { nodes = (nodes g), edges = edges g ++ (map (root a) tvs) }
  where root a' ((Step a'' _),b) = (a',a'',b)

trav1 = walk g1 (search 1 (Prim 1)) [1]

satisfyingTraversal graph agent formula s0 = walk graph (search agent formula) s0
solveGraph graph agent formula = G { nodes = nodes graph, edges = edges' }
  where graph' = spanning graph . satisfyingTraversal graph agent formula
        edges' = nub $ concat [edges $ graph' s | s <- nodes graph]
-- spanning g1 (walk g1 (search 1 (Know 1 (Prim 1))) [1])
-- => G {nodes = [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]], edges = [([1],[],1),([1],[3],1),([1],[2],1),([1],[2,3],1),([1],[1,3],1),([1],[1,2],1),([1],[1,2,3],1)]}


g2 = G { nodes = subsets [0..3], edges = [] }
fm2 = (Know 1 (Prim 1 `And` Prim 2))
test1 = map (\i -> solveGraph g2 i ((Know (i) (foldl And (Prim 0) $ map Prim [1..i])))) [1..3]


data RoseTree a = Node a [RoseTree a] deriving Show

spans vs v (Prim p)    = if p `elem` v then [v] else []
spans vs v (Neg p)     = undefined
spans vs v (And p1 p2) = spans vs v p1 `intersect` spans vs v p2
spans vs v (Know a p)  = let vs' = (filter (/=v) vs) in v : concatMap (flip (spans vs') p) vs

trave (v:vs) a (Prim p)    = Node (v,a) $ map (\v -> Node (v,a) []) vs
trave (v:vs) a (And p1 p2) = Node (v,a) $ [trave vs a p1, trave vs a p2]
trave (v:vs) a (Know a' p) = Node (v,a) $ [trave vs a' p]



