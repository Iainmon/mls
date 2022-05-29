{-# LANGUAGE NamedFieldPuns, FlexibleInstances, TupleSections #-}


module Kripke where



type Collection a = [a] -- Set a


-- isin :: Eq a => a -> Collection a -> Bool
-- isin = undefined
-- union  :: Eq a => Collection a -> Collection a -> Collection a
-- union = undefined
-- toList :: Collection a -> [a]
-- toList = undefined

-- type State = Map PrimProp Bool

type R agent state = agent -> Collection (state,state)
type V state prim  = state -> prim -> Bool


data KripkeModel agent prim state
  = M { agents :: Collection agent
      , prims  :: Collection prim
      , states :: Collection state
      , accessibility :: R agent state
      , valuation :: V state prim 
      }

instance (Show agent,Show prim,Show state) => Show (KripkeModel agent prim state) where
  show M{agents,prims,states,accessibility,valuation} = "Agents: " ++ show agents ++ "\nPrims: " ++ show prims ++ "\nStates: " ++ show states ++ "\nAccess: " ++ show [(a,rel) | a <- agents, rel <- accessibility a]



