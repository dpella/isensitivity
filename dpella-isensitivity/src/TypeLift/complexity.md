# Some complexity analysis of new trick

Imagine you have a column of ages from `1 - 100`, then for MWEM we will
synthesize 100 functions to count the number of rows (people) with certain age.

For instance,
```haskell
Age = One | Two | Three | .. | Hundred

q :: Age -> Int
q age = case age of
    One -> 1
    _   -> 0
```

## Analysis

    trick_old f ↦ [f One, f Two, f Three .. f Hundred] → {0, 1}
         total apps: 100

    trick_new f ↦ [f One, f _] → {0, 1}
         total apps: 2

    O(trick_old) ≈ O(|D|)
    O(trick_new) ≈ O(|q|)

    |q| ≤ |D| ⇒ O(trick_new) ≤ O(trick_old)

## Commentary

To figure out the range a function `f`, the old trick needs to apply `f` for each
element in the domain (100 times in this case).

Now, with our approach, the new trick use type level information to figure out that we 
can get away with applying `f` 2 times!

### Scaling
It's easier to observe the benefit in the reduced complexity over multiple functions. 

```haskell
f1 :: Age -> Int
f1 One = 1
f1 _   = 0

f2 :: Age -> Int
f2 Two = 1
f2 _   = 0

f3 :: Age -> Int
f3 Three = 1
f3 _     = 0
.
.
.
f100 :: Age -> Int
f100 Hundred = 1
f100 _       = 0
```

Since the new trick only need to apply each function 2 times, if we would like to analyse
them all we coulld get away with only making 2x100 = 200 function calls!

With the old trick, we would need to apply each function 100 times for a total of 
100x100 = 10000 evaluations.

So, we went from 10 000 evaluations to 200. Nice :)

