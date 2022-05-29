module Main where

import Lib

import Language
import Kripke
import Logic
-- import Solver

import Data.List (nubBy)



-- formula = (Know "a" ((Prim "ta") `And` (Prim "tb"))) -- `And` (Know "b" (Neg $ And (Neg $ Prim "ta") (Neg $ Prim "tb")))

-- allModels = consModels (agentsUsed formula) (primsUsed formula)

-- foundModels = nubBy sameRelation $ findModels formula


main :: IO ()
main = someFunc
