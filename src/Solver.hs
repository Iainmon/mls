{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module Solver where

import Language
import Kripke
import Logic



import Data.List (inits, tails, nub, permutations, nubBy, isPrefixOf, sort)


-- import Data.List (inits, tails, nub, permutations, nubBy, isPrefixOf, sort)

-- import Data.Set (Set)
-- import qualified Data.Set as Set


-- type State prim = ([prim],[prim]) -- (truths, falsehoods)

-- type KripkeStar agent prim = KripkeModel agent prim (State prim)

-- realWorld = ([],[])

-- decide :: Eq prim => State prim -> prim -> Bool
-- decide (trus,flss) p | p `elem` trus = True
--                      | p `elem` flss = False
--                      | trus == [] && flss == [] = True 
--                      | otherwise     = undefined

-- partitions :: [a] -> [[[a]]]
-- partitions = foldr (\x r -> r >>= bloat x) [[]]
--   where bloat x  []      = [[[x]]]
--         bloat x (xs:xss) = ((x:xs):xss) : map (xs:) (bloat x xss)

-- bipartitions :: [a] -> [[[a]]]
-- bipartitions = filter ((==) 2 . length) . partitions

-- consStates :: Eq prim => [prim] -> [State prim]
-- -- consStates xs = [(as,bs) | [as,bs] <- bipartitions xs]
-- consStates xs = [(a,b) | b <- subsets xs,
--                          a <- subsets xs,
--                          a ++ b `elem` permutations xs]


-- consModel :: (Eq agent, Eq prim) => [agent] -> [prim] -> R agent (State prim) -> KripkeModel agent prim (State prim)
-- consModel ags prms accessR = M { agents       = ags
--                               , prims         = prms
--                               , states        = consStates prms
--                               , valuation     = decide
--                               , accessibility = accessR
--                               }
            

-- subsets :: [a] -> [[a]]
-- subsets []  = [[]]
-- subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- consRelations :: Eq state => [state] -> state -> [[(state,state)]]
-- consRelations states s = map (map (s,)) $ subsets states

-- functionify :: Eq a => [(a,b)] -> a -> b
-- functionify [] _ = undefined
-- functionify ((x,y):xys) x' | x == x'   = y
--                            | otherwise = functionify xys x'



-- consModels :: (Eq agent, Ord prim) => [agent] -> [prim] -> [KripkeStar agent prim]
-- consModels ags prms = map (consModel ags prms) relations
--   where relations     = map functionify $ [ss | ss <- subsets relationPairs, (nub $map fst ss) `elem` permutations (nub ags)]-- length (map fst ss) == length ags]
--         relationPairs = nub [(a,rel) | rel <- rels, a <- ags]
--         valid rel = length ags == length (map fst rel) -- (map fst rel) `elem` permutations ags
--         states = consStates prms
--         rels = consRelations states realWorld

-- findModels :: (Eq ag, Ord at) => L ag at -> [KripkeStar ag at]
-- findModels formula = filter satisfies models
--   where models = consModels (agentsUsed formula) (primsUsed formula)
--         satisfies m = sem m realWorld formula


-- sameRelation :: (Ord ag, Ord at) => KripkeStar ag at -> KripkeStar ag at -> Bool
-- sameRelation k1 k2 = Set.fromList [(a,s) | a <- agents k1,s <- accessibility k1 a] == Set.fromList [(a,s) | a <- agents k2,s <- accessibility k2 a]
