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