module Lib where


import Data.List (inits, tails, nub, permutations, nubBy, isPrefixOf, sort)


remove :: String -> String -> String
remove w "" = ""
remove w s@(c:cs) 
  | w `isPrefixOf` s = remove w (drop (length w) s)
  | otherwise = c : remove w cs
  
intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []
intersect (x:xs) l | elem x l = x : intersect xs l
                   | otherwise = intersect xs l
