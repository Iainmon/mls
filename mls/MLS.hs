{-# LANGUAGE NamedFieldPuns, TupleSections #-}

module MLS where

import System.Process

import Data.List (nub,findIndex,isPrefixOf)

data L ag at 
  = Prim at
  | Neg (L ag at) 
  | And (L ag at) (L ag at) 
  | Know ag (L ag at)
  -- | Believe ag (L ag at)

instance (Show ag,Show at) => Show (L ag at) where
  show (Prim p) = show p
  show (Neg p)  = "¬ (" ++ show p ++ ") "
  show (And p1 p2) = show p1 ++ " ⋀ " ++ show p2
  show (Know a p)  = "K<" ++ show a ++ ">(" ++ show p ++ ")"
  -- show (Believe a p) = "B<" ++ show a ++ ">[" ++ show p ++ "]"

agentsUsed :: Eq ag => L ag at -> [ag]
agentsUsed (Prim _) = []
agentsUsed (Neg phi) = agentsUsed phi
agentsUsed (And p1 p2) = nub $ agentsUsed p1 ++ agentsUsed p2
agentsUsed (Know a p) = nub $ a : agentsUsed p
-- agentsUsed _ = undefined

primsUsed :: Eq at => L ag at -> [at]
primsUsed (Prim p) = [p]
primsUsed (Neg phi) = primsUsed phi
primsUsed (And p1 p2) = nub $ primsUsed p1 ++ primsUsed p2
primsUsed (Know _ p) = primsUsed p
-- primsUsed _ = undefined

modalStrip :: L ag at -> L ag at
modalStrip (Prim p) = Prim p
modalStrip (Neg p)  = Neg $ modalStrip p
modalStrip (And p1 p2) = And (modalStrip p1) (modalStrip p2)
modalStrip (Know _ p)    = modalStrip p
-- modalStrip (Believe _ p) = modalStrip p

agentKnowledge :: Eq ag => L ag at -> ag -> [L ag at]
agentKnowledge (Prim p) _ = [Prim p]
agentKnowledge (Neg p) a  = map Neg $ agentKnowledge p a
agentKnowledge (And p1 p2) a = concatMap (flip agentKnowledge a) [p1,p2]
agentKnowledge (Know a p)  a' | a == a'   = agentKnowledge p a
                              | otherwise = []

peval :: L ag at -> (at -> Bool) -> Bool
peval (Prim p) val    = val p
peval (Neg p) val     = not $ peval p val
peval (And p1 p2) val = peval p1 val && peval p2 val



type Collection a = [a] 

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

decide :: Eq prim => State prim -> prim -> Bool
decide = flip elem

subsets :: [a] -> [[a]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

consKS :: (Eq agent, Eq prim) => [agent] -> [prim] -> AccessRelation agent (State prim) -> KripkeModel agent prim (State prim)
consKS ags pps access = M { agents        = ags
                          , prims         = pps
                          , states        = subsets pps
                          , valuation     = decide
                          , accessibility = access
                          }

reflexiveKM :: (Eq agent, Eq prim) => [agent] -> [prim] -> KripkeModel agent prim (State prim)
reflexiveKM ags pms = consKS ags pms ac
  where ac _ = [(a,a) | a <- subsets pms]




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





models :: Eq state => KripkeModel agent prim state -> state -> L agent prim -> Bool
models model@(M{agents,prims,states,accessibility,valuation}) s phi = sem' phi
  where models' = models model -- entails
        sem' (Prim pr)   = valuation s pr
        sem' (Neg phi)   = not $ models' s phi
        sem' (And p1 p2) = models' s p1 && models' s p2
        sem' (Know a p)  = if null $ accessibility a then False else and [models' t p | (s',t) <- accessibility a, s == s']
        -- sem' (Believe a p)  = and [models' t p | (s',t) <- accessibility a, s == s']

genKripke :: (Eq ag,Eq at) => L ag at -> (KripkeModel ag at (State at),[State at])
genKripke phi = (km',ss)
  where (km,ss) = satKM phi
        km' = km { accessibility = nub . ac' }
        ac' a = reflize [(s,t) | s <- ss,
                         t <- states km,
                         and [ satState (valuation km) t phi' | phi' <- agentKnowledge phi a]
                         ]
        reflize xs = [(x,x) | (_,x) <- xs] ++ xs

entails :: (Eq agent,Eq prim) => L agent prim -> L agent prim -> Bool
entails p p' = and $ map (flip (models km) p') ss
    where (km,ss) = genKripke p

(|=) :: Eq state => (KripkeModel agent prim state, state) -> L agent prim -> Bool 
(|=) (km,s) phi = models km s phi











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

-- Examples in paper:
formula = Know 'a' (Prim 1 `And` Prim 2) `And` Know 'b' (Prim 2)
-- formula = Know 'a' (Prim 1 `And` Prim 2) `And` Know 'b' (Prim 2) `And` Know 'c' (Prim 3)

tests = [ Know 'a' (Prim 1)
        , Know 'a' (Prim 2)
        , Neg $ Know 'b' $ Neg $ Prim 2
        , Neg $ Know 'b' (Prim 1)
        ]




main = do
          let (km,ss) = genKripke formula
          printDOTs formula $ map kripkeToDOTGraph [km]





exportKripkeGraph km = do { printDOTs formula $ map kripkeToDOTGraph [km] }



remove :: String -> String -> String
remove w "" = ""
remove w s@(c:cs) 
  | w `isPrefixOf` s = remove w (drop (length w) s)
  | otherwise = c : remove w cs

findIndex' a b = case findIndex a b of { (Just i) -> i }

kripkeToDOTGraph m = remove "\\\"" $ "digraph G {\nnode [colorscheme=paired9 style=\"filled\"]\nedge [colorscheme=set19]\n" ++ concat (map (\s -> s ++ ";\n") nodesStmts) ++ concat (map (\s -> s ++ ";\n") edgeStmts) ++ "}" 
  where nodesStmts = [ show (show s) ++ " [label=" ++ show (show s) ++ " color=1" ++ "]" | s <- states m]
        cone (a,b) ag = show (show a) ++ "  -> " ++ show (show b) ++ " [label=" ++ show (show ag) ++ " color=" ++ show ((+1) $ findIndex' (ag==) (agents m)) ++ "]"
        edgeStmts = [cone ss ag | ag <- agents m, ss <- accessibility m ag]

printDOTs fm ms 
  = do system "rm -rf out; mkdir out"
       mapM_ system $ map (\(i,gs) -> "echo '" ++ gs ++ "' | dot -Tsvg > out/"++ show i ++".svg -Glabel=\"" ++ show fm ++ "\"") (zip [1..length ms] ms)
