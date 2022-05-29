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


type AccessRelation agent state = agent -> Collection (state,state)
type Valuation state prim = state -> prim -> Bool

type R agent state = AccessRelation agent state
type V state prim  = Valuation state prim

data KripkeModel agent prim state
  = M { agents        :: Collection agent
      , prims         :: Collection prim
      , states        :: Collection state
      , accessibility :: AccessRelation agent state -- R agent state
      , valuation     :: Valuation state prim       -- V state prim
      }

instance (Show agent,Show prim,Show state) => Show (KripkeModel agent prim state) where
  show M{agents,prims,states,accessibility,valuation} = "Agents: " ++ show agents ++ "\nPrims: " ++ show prims ++ "\nStates: " ++ show states ++ "\nAccess: " ++ show [(a,rel) | a <- agents, rel <- accessibility a]






type State prim = Collection prim

type KripkeStar agent prim = KripkeModel agent prim (State prim)

type KSAccessRelation agent prim = AccessRelation agent (State prim)

decide :: Eq prim => State prim -> prim -> Bool
decide = flip elem

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)


-- Construct a Kripke Star Model (KSM), 
--   given a set of agents,
--   primitive propositions,
--   and an accessibility relation
consKS :: (Eq agent, Eq prim) => [agent] -> [prim] -> AccessRelation agent (State prim) -> KripkeModel agent prim (State prim)
consKS ags pps access = M { agents        = ags
                          , prims         = pps
                          , states        = subsets pps
                          , valuation     = decide
                          , accessibility = access
                          }

-- Type of a specific agent's accessibility relation for a Kripke Star Model (KSM).
-- Equivalently, a pair of an agent, and the set of worlds the agent considers from the real world.
-- newtype KSConsiders agent prim 
--   = Consider { who    :: agent
--              , worlds :: Collection (State prim)
--              }


-- A pair of a world and the set of agents that consider that world from the current state in a KSM.
type KSEdge agent prim = (State prim,Collection agent)


type KSEdges agent prim = Collection (KSEdge agent prim) -- [([prim],[agent])]


edgesToKSAccess :: (Eq agent,Eq prim) => State prim -> KSEdges agent prim -> KSAccessRelation agent prim
edgesToKSAccess rw edges agent = map (rw,) $ map fst $ filter (elem agent . snd) edges



-- Given a set of primitive propositions, construct every state in the graph
everyState :: Eq prim => Collection prim -> Collection (State prim)
everyState = subsets

-- Given a world, and a set of agents, construct every way that world could be considered.
everyLabel :: (Eq agent,Eq prim) => State prim -> [agent] -> [(State prim,[agent])]
everyLabel world = map (world,) . subsets



allEdges :: (Eq agent,Eq prim) => Collection (State prim) -> Collection agent -> Collection (KSEdges agent prim)
allEdges worlds agents = go worlds
  where go []     = [[]]
        go (w:ws) = concat [map (:fx) (map (w,) agps) | fx <- go ws]
        agps = subsets agents

allAccess :: (Eq agent,Eq prim) => Collection prim -> Collection agent -> State prim -> Collection (KSAccessRelation agent prim)
allAccess prims agents state = map (edgesToKSAccess state) relations
  where states    = everyState prims
        relations = allEdges states agents


everyKSM :: (Eq agent,Eq prim) => Collection prim -> Collection agent -> State prim -> Collection (KripkeStar agent prim)
everyKSM prims agents state = map (consKS agents prims) accesses
  where accesses = allAccess prims agents state



