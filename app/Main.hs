module Main where

import Lib

import Language
import Kripke
import Logic
import Solver
import Finder

import System.Process

import Data.List (nubBy,findIndex)





p1 = Prim "p1"
p2 = Prim "p2"
p3 = Prim "p3"
ka = Know "a"
kb = Know "b"
kc = Know "c"


-- Presentation examples:
-- formula = (ka (p1 `And` p2) `And` (kb p2))
-- formula = (ka (p1 `And` p2) `And` (kb (p2 `And` p1)))
-- formula = (ka (p1 `And` p2) `And` (kb (p2 `And` p1)) `And` (kc p3))
formula = Know 'a' (Prim 1 `And` Prim 2) `And` Know 'b' (Prim 2) `And` Know 'c' (Prim 3)

test1 = ka p1
test2 = ka p2
test3 = Neg $ kb $ Neg $ p2
test4 = kb p1

(km,ss) = satKM' formula 
evalF = evaluate formula


















-- formula = (ka (p1 `And` p2 `And` p3)) `And` (kb p2) `And` (kc p3)
-- formula = (ka (p1 `And` p2)) `And` (kb (p2 `And` p3)) `And` (kc (p1 `And` p3))
-- formula = (Know "a" ((Prim "ta") `And` (Prim "tb"))) `And` (Know "b" (Prim "ta")) -- (Know "b" (Neg $ And (Neg $ Prim "ta") (Neg $ Prim "tb")))


findIndex' a b = case findIndex a b of { (Just i) -> i }

conjoin [] = undefined
conjoin [x] = x `And` x
conjoin (x:xs) = x `And` (conjoin xs)

-- formula = conjoin $ zipWith Know ags clauses
--   where clauses = map conjoin $ [cls | cls <- subsets $ pms, length cls == 2]
--         pms = map (Prim . ('p':) . show) [1..4]
--         ags = map (:[]) ['a'..'c']

-- km = unconnectedKM [1,2] [1,2]


f m = sat m [] (Know 1 $ Prim 1)

foundModels = findModels formula

-- kripkeToDOTGraph' m = "digraph G {" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
--   where nodesStmts = [ show (show s) ++ " [label=" ++ show (show s) ++ "]" | s <- states m]
--         cone (a,b) ag = show (show a) ++ "  -> " ++ show (show b) ++ " [label=" ++ show (show ag) ++ "]"
--         edgeStmts = [cone ss ag | ag <- agents m, ss <- accessibility m ag]


kripkeToDOTGraph' m = remove "\\\"" $ "digraph G {\nnode [colorscheme=paired9 style=\"filled\"]\nedge [colorscheme=set19]\n" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
  where nodesStmts = [ show (show s) ++ " [label=" ++ show (show s) ++ " color=1" ++ "]" | s <- states m]
        cone (a,b) ag = show (show a) ++ "  -> " ++ show (show b) ++ " [label=" ++ show (show ag) ++ " color=" ++ show ((+1) $ findIndex' (ag==) (agents m)) ++ "]"
        edgeStmts = [cone ss ag | ag <- agents m, ss <- accessibility m ag]

kripkeToDOTGraph'' m = remove "\\\"" $ "digraph G {\nnode [colorscheme=paired9 style=\"filled\"]\nedge [colorscheme=set19]\n" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
  where nodesStmts = [ show (show s) ++ " [label=" ++ show (show s) ++ " color=1" ++ "]" | s <- nodes m]
        cone (a,b) ag = show (show a) ++ "  -> " ++ show (show b) ++ " [label=" ++ show (show ag) ++ "]"
        edgeStmts = [cone (v,w) l | (v,w,l) <- edges m]

printDOTs fm ms 
  = do system "rm -rf out; mkdir out"
       mapM_ system $ map (\(i,gs) -> "echo '" ++ gs ++ "' | dot -Tsvg > out/"++ show i ++".svg ") (zip [1..length ms] ms)
      --  mapM_ system $ map (\(i,gs) -> "echo '" ++ gs ++ "' | dot -Tsvg > out/"++ show i ++".svg -Glabel=\"" ++ show fm ++ "\"") (zip [1..length ms] ms)

main :: IO ()
main = do 
          -- putStrLn $ kripkeToDOTGraph' $fst (satKM' formula)
          printDOTs formula $ map kripkeToDOTGraph' [fst (satKM' formula)]
          -- printDOTs fm2 $ map kripkeToDOTGraph'' test1
        --   let models = everyKSM (primsUsed formula) (agentsUsed formula) realWorld
        --   putStrLn $ show $ length models
        --   putStrLn $ show $ length foundModels
        --   let foundModels = reverse $ findModels formula
        --   -- mapM_ putStrLn $ map kripkeToDOTGraph' (foundModels)
        --   printDOTs formula $ map kripkeToDOTGraph' foundModels
        --   putStrLn $ show $ length foundModels



-- satKM' :: (Eq ag,Eq at) => L ag at -> (KripkeModel ag at (State at),[State at])

-- genKripke :: (Eq ag,Eq at) => L ag at -> (KripkeModel ag at (State at),[State at])

