{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}


module Logic where

import Lib
import Language
import Kripke





-- Make this into a typeclass somehow? or parts of it?
sem :: Eq state => KripkeModel agent prim state -> state -> L agent prim -> Bool
sem model@(M{agents,prims,states,accessibility,valuation}) s phi = sem' phi
  where models = sem model -- entails
        sem' (Prim pr)   = valuation s pr
        sem' (Neg phi)   = not $ models s phi
        sem' (And p1 p2) = models s p1 && models s p2
        sem' (Know a p)  = if null $ accessibility a then False else and [models t p | (s',t) <- accessibility a, s == s']
        sem' (Believe a p)  = and [models t p | (s',t) <- accessibility a, s == s']


sat :: Eq state => KripkeModel agent prim state -> state -> L agent prim -> [state]
sat model@(M{agents,prims,states,accessibility,valuation}) s phi = sat' phi
  where satisfies = sat model -- entails
        sat' (Prim pr)   = [s' | s' <- states, valuation s' pr]
        sat' (Neg p)   = sat (model { states = (filter (/=s) $ states) }) s p
        sat' (And p1 p2) = satisfies s p1 `intersect` satisfies s p2
        sat' (Know a p)  = xset -- [s' | s' <- states, not $ s' `elem` xset]
          where xset = [t | t <- states, 
                            s' <- satisfies s p,
                            (s',t) `elem` accessibility a]
        -- sat' (Believe a p)  = and [models t p | (s',t) <- accessibility a, s == s']










