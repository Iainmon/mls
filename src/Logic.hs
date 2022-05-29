{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}


module Logic where


import Language
import Kripke





-- Make this into a typeclass somehow? or parts of it?
sem :: Eq state => KripkeModel agent prim state -> state -> L agent prim -> Bool
sem model@(M{agents,prims,states,accessibility,valuation}) s phi = sem' phi
  where models = sem model -- entails
        sem' (Prim pr)   = valuation s pr
        sem' (Neg phi)   = not $ models s phi
        sem' (And p1 p2) = models s p1 && models s p2
        sem' (Know a p)  = and [models t p | (s',t) <- accessibility a, s == s']
        sem' (Believe a p)  = and [models t p | (s',t) <- accessibility a, s == s']



