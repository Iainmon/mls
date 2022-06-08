# Modal Logic Simulator


To load the implementation, run

```bash
$ ghci MLS.hs
```

you can then test entailment of formulae via

```haskell
ghci> formula = Know 'a' (Prim 1 `And` Prim 2) `And` Know 'b' (Prim 2)
ghci> formula `entails` (Know 'a' (Prim 1))
```

you can generate a pair of a Kripke Model and list of satisfying states, given a formula

```haskell
ghci> (km,s:ss) = genKripke formula
```

then you can test other formulae against your new Kripke Model (or one you already defined)

```haskell
ghci> (km,s) |= (Know 'a' (Prim 1))
```

and if you run `brew install graphviz` (for Mac), otherwise install via https://graphviz.org/download/, you can generate a graphical representation of a Kripke Model by running

```haskell
ghci> exportKripkeGraph km
```

which will save the graph representation to the file `.out/1.svg`.


