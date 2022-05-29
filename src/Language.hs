


module Language where

import Data.List (nub)

data L ag at 
  = Prim at
  | Neg (L ag at) 
  | And (L ag at) (L ag at) 
  | Know ag (L ag at)
  | Believe ag (L ag at)

instance (Show ag,Show at) => Show (L ag at) where
  show (Prim p) = show p
  show (Neg p)  = "¬ " ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " ⋀ " ++ show p2 ++ ")"
  show (Know a p)  = "K<" ++ show a ++ ">[" ++ show p ++ "]"
  show (Believe a p) = "B<" ++ show a ++ ">[" ++ show p ++ "]"

agentsUsed :: Eq ag => L ag at -> [ag]
agentsUsed (Prim _) = []
agentsUsed (Neg phi) = agentsUsed phi
agentsUsed (And p1 p2) = nub $ agentsUsed p1 ++ agentsUsed p2
agentsUsed (Know a p) = nub $ a : agentsUsed p
agentsUsed _ = undefined

primsUsed :: Eq at => L ag at -> [at]
primsUsed (Prim p) = [p]
primsUsed (Neg phi) = primsUsed phi
primsUsed (And p1 p2) = nub $ primsUsed p1 ++ primsUsed p2
primsUsed (Know _ p) = primsUsed p
primsUsed _ = undefined
