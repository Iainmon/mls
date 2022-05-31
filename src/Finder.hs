
{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}


module Finder where

import Lib
import Language
import Kripke

peval :: L ag at -> (at -> Bool) -> Bool
peval (Prim p) val    = val p
peval (Neg p) val     = not $ peval p val
peval (And p1 p2) val = peval p1 val && peval p2 val


modalStrip :: L ag at -> L ag at
modalStrip (Prim p) = Prim p
modalStrip (Neg p)  = Neg $ modalStrip p
modalStrip (And p1 p2) = And (modalStrip p1) (modalStrip p2)
modalStrip (Know _ p)    = modalStrip p
modalStrip (Believe _ p) = modalStrip p



satState :: Valuation state at -> state -> L ag at -> Bool
satState val s = flip peval (val s)

satStates :: Valuation state at -> [state] -> L ag at -> [state]
satStates val ss phi = filter (flip (satState val) (modalStrip phi)) ss


startStates :: KripkeModel ag at state -> L ag at -> [state]
startStates M{agents,prims,states,accessibility,valuation} = satStates valuation states

satKM :: (Eq ag,Eq at) => L ag at -> (KripkeModel ag at (State at),[State at])
satKM phi = (km',ss)
  where km = reflexiveKM (agentsUsed phi) (primsUsed phi)
        ss = startStates km phi
        km' = km { accessibility = ac' }
        ac' _ = zip ss ss

