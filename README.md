# Accompanying code 

Paper: Pettersson M., Ekeroth, J. L., and Russo, A. Calculating Function
Sensitivity for Synthetic Data Algorithms, ACM 35th Symposium on Implementation
and Application of Functional Languages (IFL), 2023. 


## The improved version of the trick

To try `trick'`, then 

```bash
cabal repl dpella-isensitivity
```

and once in the Haskell's prompt: 

```
*DSL.Deep> :module TypeLift.Trick

Prelude TypeLift.Trick> :t foo
foo :: Num p => PatternsOf T '[(), o2, o3] -> p

Prelude TypeLift.Trick> :t trick
trick
  :: Data.SOP.Constraint.All HasBeenMatched ts =>
     (PatternsOf t ts -> r) -> [r]

Prelude TypeLift.Trick> trick foo
[10,1]
```

## To run MWEM


```bash 
cabal repl dpella-benchmark
```

and then, from the Haskell's prompt: 

```
*Main> main
producing (1.0e-2,10) ..
producing (0.1,10) ..
producing (1.0,10) ..
ByeBye
```

The specification of the workload using the improved version of the `trick` is
in the file `./dpella-benchmark/app/Workload.hs`. The generated synthetic
datasets are in the `results` folder. 

## Disclaimer 

This code is for demonstration purposes. It is not intended to be used in
production.