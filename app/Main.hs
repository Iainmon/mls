module Main where

import Lib

import Language
import Kripke
import Logic
import Solver

import System.Process

import Data.List (nubBy,findIndex)





findIndex' a b = case findIndex a b of { (Just i) -> i }



formula = (Know "a" ((Prim "ta") `And` (Prim "tb"))) `And` (Know "b" (Prim "ta")) -- (Know "b" (Neg $ And (Neg $ Prim "ta") (Neg $ Prim "tb")))


foundModels = findModels formula

-- kripkeToDOTGraph' m = "digraph G {" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
--   where nodesStmts = [ show (show s) ++ " [label=" ++ show (show s) ++ "]" | s <- states m]
--         cone (a,b) ag = show (show a) ++ "  -> " ++ show (show b) ++ " [label=" ++ show (show ag) ++ "]"
--         edgeStmts = [cone ss ag | ag <- agents m, ss <- accessibility m ag]


kripkeToDOTGraph' m = remove "\\\"" $ "digraph G {\nnode [colorscheme=paired9 style=\"filled\"]\nedge [colorscheme=set19]\n" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
  where nodesStmts = [ show (show s) ++ " [label=" ++ show (show s) ++ " color=1" ++ "]" | s <- states m]
        cone (a,b) ag = show (show a) ++ "  -> " ++ show (show b) ++ " [label=" ++ show (show ag) ++ " color=" ++ show ((+1) $ findIndex' (ag==) (agents m)) ++ "]"
        edgeStmts = [cone ss ag | ag <- agents m, ss <- accessibility m ag]

printDOTs fm ms 
  = do system "rm -rf out; mkdir out"
       mapM_ system $ map (\(i,gs) -> "echo '" ++ gs ++ "' | dot -Tpng > out/"++ show i ++".png -Glabel=\"" ++ show fm ++ "\"") (zip [1..length ms] ms)

main :: IO ()
main = do 
          let foundModels = reverse $ findModels formula
          mapM_ putStrLn $ map kripkeToDOTGraph' (foundModels)
          printDOTs formula $ map kripkeToDOTGraph' foundModels