# Sensitivity computation for user-defined functions in Differential Privacy systems

This thesis will be carried out at [DPella AB](www.dpella.io), Kungsgatan 11 - 6th floor, 41119
Gothenburg.

- DPella AB supervisor: Marco Gaboardi
- Chalmers supervisor: Prof. Alejandro Russo
- Possible examiner: Prof. David Sands

Skills required to complete the thesis:
- Design and implementation of DSLs in Haskell (a great advantage if you took
  TDA342/DIT260)

## Differential Privacy

[Differential privacy](https://link.springer.com/chapter/10.1007/11681878_14)
(DP) is emerging as a viable solution to release statistical information about
the population without compromising data subjects' privacy. A standard way to
achieve DP is by adding some statistical noise to the result of a data analysis.
If the noise is carefully calibrated, it provides *privacy* protection for the
individuals contributing their data.

## Calibrated noise

A standard way to achieve DP is adding some carefully calibrated noise to the
result of a query. To protect all the different ways in which an individual's
data can affect the result of a query, the noise needs to be calibrated to the
maximal change that the result of the query can have when changing an
individual's data. This is formalized through the notion of **sensitivity**. The
sensitivity gives a measure of the amount of noise needed to protect one
individual's data.

## Queries as functions

Queries on a given table can be seen as functions of the form:

```haskell
query :: Table -> Result
```

For simplicity, we assume that the type `Table` is simply a list of records. To
give a concrete example about how a query can look like, let us assume
following table of allergic people at work,

| name | allergic |
| ---- | ------- |
| Alejandro | 1 |
| Marco     | 0 |
| Elisabet  | 1 |

where we assume that

```haskell
type Table = [Row]

data Row = MkRow
    {
      name    :: String
    , alergic :: Int
    }
```

In this example, we can write a query that computes the number of allergic people as follows:

```haskell
type Result = Int
countAllergic :: Table -> Int
countAllergic = sum . map alergic
```

## Reasoning about sensitivity of functions

To reason about sensitivity, we need to reason about what is the magnitude of
change that a query suffers when a record gets removed from the table.

The function `countAllergic` returns a count of `2` in the table above. However,
if we remove an individual and we consider the following table instead:

| name | allergic |
| ---- | ------- |
| Alejandro | 1 |
| Marco     | 0 |

`countAllergic` returns `1`. It seems that the variability is just about `1` --
you can verify that the same phenomenon occurs if you remove any other row in
the table we described first.

We can say that `countAllergic` has sensitivity **1**, but why? Observe that the
type of `countAllergic` indicates the co-domain are all the integers:

```haskell
countAlergic :: Table -> Int
```

but the range of the function is the set `{0,1}`! -- a range admits a
variability of 1. So, we know that if we remove one row from the table, then the
result will change, at most, by 1.

# Goal of the thesis

The overall goal of to create a domain specific language where users can write
functions (queries) and an interpreter can not only run them but also
approximate their range. We will use the result of this thesis to encode scoring
functions of the [exponential mechanism (see page
37)](https://www.cis.upenn.edu/~aaroth/Papers/privacybook.pdf).

More concretely, the thesis should enable developers of differential privacy
system to write query of the type:

```haskell
query :: Num a => Table -> DSL a
```

where the type `DSL` denotes the domain specific language conceived in this
thesis, and the type constraint `Num a` indicates that the type `a` support a
notion of interval to capture the range of the query. Once queries are written
in such DSL, the thesis should also provide two run functions:

```haskell
run   :: Num a => (Table -> DSL a) -> Table -> a
range :: Num a => (Table -> DSL a) -> Range a a
```

Function `run` executes the program and produces a result, e.g., `run
countAllergic table` returns the number of allergic people in `table`.
Instead, `range countAllergic` returns the range of the function, i.e., the
interval `[0 .. 1]`.

There are several challenges aspects about writing the function `range`.

## Data independent range analysis

To analyze the possible values return by a query `q :: Table -> DSL a`, the
function `range` needs to do it in a data independent manner. *The analysis
should not depend on the content of the table but its schema of it like the
possible values the column might assume.* This requirement will demand to
introduce symbolic values (denoting any possible table) when defining `range`
and/or processing intermediate tables.

## Interval analysis

The function `range` needs to interpret the query within a interval semantics.
For instance, if the body of the query adds two variables

```haskell
query table =
   ...
   z <- x + y
   ...
```

where `x` is in the range `[0,10]` and `y` is in the range `[1,5]`, then `z`
should be given the semantics of being in the range `[0,15]`. Fortunately, there
are a couple of libraries in Haskell which support interval semantics:

- [range: An efficient and versatile range library](https://hackage.haskell.org/package/range)
- [data-interval: Interval datatype, interval arithmetic, and interval-based containers](https://hackage.haskell.org/package/data-interval)

Part of the thesis contribution would be to pick one of them and justify why is
the adequate one for this problem.

## Branches

Another challenge aspect of this work is how to deal with branches done by
queries. For instance, imagine a query that does the following:

```haskell
query table =
   ...
   x <- some_computation
   if x > 0 then x
            else y
```

Assuming that `y` is in the range `[3,7]`, we have that
* If `x` is in the range `[-5,-1]`, then the range of the query gets reduce to
`[3,7]`, i.e., we only need to consider the `else`-branch;
* If `x` is in the range `[1,5]`, then the range of the query gets reduce to
  `[1,5]`, i.e., we only consider the `then`-branch;
* If `x` is in the range `[-5,5]`, then the range of the query is `[-5,7]`,
  i.e., we consider both branches!

So, the `range` function needs to detect and consider the possible range on the
condition of branches and then calculate the range of the conditional based on
the considered branches.

There are many ways to capture branches on embedded DSLs, one of the most
naturally is to use Haskell functions itself, but that demands to use (partial
evaluation) tricks to *penetrate* such functions and analyze their body.
Fortunately, there are some papers about it:
- [Towards secure IoT programming in
  Haskell](https://dl.acm.org/doi/10.1145/3406088.3409027)
- [Pattern Matching for Non-inductive Types in Code-genering Haskell EDSLs](https://gupea.ub.gu.se/bitstream/2077/69659/1/gupea_2077_69659_1.pdf)

## Integration into DPella's backend

Part of the thesis will consists on integrating the result of this work for the
exponential mechanism implementation at DPella.
