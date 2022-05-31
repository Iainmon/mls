# mls - Modal Logic Solver


works. run:

```haskell
ghci> putStrLn $ kripkeToDOTGraph' $ fst (satKM formula)
```

or run 

```haskell
ghci> putStrLn $ kripkeToDOTGraph' $ fst (satKM' formula)
```

for nontrivial kripke models.

Example:

```haskell
ghci> formula
((K<"a">[("p1" ⋀ "p2")] ⋀ K<"b">[("p2" ⋀ "p3")]) ⋀ K<"c">[("p1" ⋀ "p3")])
ghci> km = fst (satKM' formula)
ghci> km
Agents: ["a","b","c"]
Prims: ["p1","p2","p3"]
States: [[],["p3"],["p2"],["p2","p3"],["p1"],["p1","p3"],["p1","p2"],["p1","p2","p3"]]
Access: [("a",(["p1","p2"],["p1","p2"])),("a",(["p1","p2","p3"],["p1","p2","p3"])),("a",(["p1","p2","p3"],["p1","p2"])),("b",(["p2","p3"],["p2","p3"])),("b",(["p1","p2","p3"],["p1","p2","p3"])),("b",(["p1","p2","p3"],["p2","p3"])),("c",(["p1","p3"],["p1","p3"])),("c",(["p1","p2","p3"],["p1","p2","p3"])),("c",(["p1","p2","p3"],["p1","p3"]))]
ghci> s0 = ["p1","p2","p3"]
ghci> sem km s0 formula
True
ghci> ka p1
K<"a">["p1"]
ghci> sem km ["p1","p2","p3"] (ka p1)
True
ghci> 
```