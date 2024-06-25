
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes#-}
module DSL.Deep
  ( Value
  , lit, eCase, qif
  , (==:), (/=:), (<:), (>:), (<=:), (>=:), (||:), (&&:)
  , intervalSem, range, run
  , Query
  , oneAttr, twoAttr, fourAttr
  , runQuery
  , intervalSemQuery, rangeQuery
  -- Re-exports
  , module TypeLift.TH.STGen
  , Trick.Matched (..), Trick.TTup (..)
  , Reifyable (..)
  ) where
import Numeric.Interval ( Interval, hull, inf, singleton, sup, width, (...) )
import Data.SOP (All)
import TypeLift.Trick ( trick, PatternsOf, HasBeenMatched, Reifyable (..) )
import qualified TypeLift.Trick as Trick
import TypeLift.TH.STGen

-- * Data types

-- | A small DSL which enables whole-program interval semantics.
data Value r where
  Lit   :: r -> Value r
  ECase :: (All HasBeenMatched ts, Num r, Ord r, Reifyable t ts) => PatternsOf t ts -> Query t r -> Value r
  -- | Conditional
  EIf :: Value Bool -> Value r -> Value r -> Value r
  -- | Binary operators
  ECompare :: Ord r => Ordering -> Value r -> Value r -> Value Bool
  EGate    :: Gate (Value Bool) -> Value Bool

data Query a r where
  OneAttr :: (All HasBeenMatched ts, Num r, Ord r, Reifyable t ts) => (PatternsOf t ts -> Value r) -> Query t r
  TwoAttr :: ( Num r, Ord r , All HasBeenMatched ts1, All HasBeenMatched ts2 , Reifyable t1 ts1, Reifyable t2 ts2)
        => (PatternsOf t1 ts1 -> PatternsOf t2 ts2 -> Value r)
        -> Query (t1, t2) r
  FourAttr :: ( Num r, Ord r
           , All HasBeenMatched ts1, All HasBeenMatched ts2, All HasBeenMatched ts3, All HasBeenMatched ts4
           , Reifyable t1 ts1, Reifyable t2 ts2, Reifyable t3 ts3, Reifyable t4 ts4)
        => (PatternsOf t1 ts1 -> PatternsOf t2 ts2 -> PatternsOf t3 ts3 -> PatternsOf t4 ts4 -> Value r)
        -> Query (t1, t2, t3, t4) r

data Gate a where
  Or  :: a -> a -> Gate a
  And :: a -> a -> Gate a
  Not :: a -> Gate a

-- * Semantics

-- | Run a program with normal run semantics.
run :: Value r -> r
run (Lit r)           = r
run (ECase i q)       = runQuery q (deify i)
run (EIf p l r)       = run (if run p then l else r)
run (ECompare op a b) = compare (run a) (run b) == op
run (EGate (And a b)) = run a && run b
run (EGate (Or a b))  = run a || run b
run (EGate (Not a))   = not (run a)

-- | Analyse a program with interval semantics.
intervalSem :: Value r -> Interval r
intervalSem (ECase _ p) = intervalSemQuery p
intervalSem (EIf p l r) =
  if run p
  then intervalSem l
  else intervalSem r
intervalSem (EGate gate) = intervalSemGate gate
intervalSem c = singleton $ run c -- For simpler constructors we adhere to the normal run semantics.

-- | Interval semantics is well defined on the query dsl.
intervalSemQuery :: Query a r -> Interval r
-- | Apply the trick and compile the results into a single interval.
intervalSemQuery (OneAttr q) = gather . trick $ q
intervalSemQuery (TwoAttr q) =
  let ras = trick q
      rbs = concatMap trick ras
  in gather rbs
intervalSemQuery (FourAttr q) =
  let ras = trick q
      rbs = concatMap trick ras
      rcs = concatMap trick rbs
      rds = concatMap trick rcs
  in gather rds

-- | Recursively apply the interval semantics to the program and compile the results into a single interval.
gather :: Ord r => [Value r] -> Interval r
gather = foldl1 hull . map intervalSem

-- | Define interval semantics for logic gates
intervalSemGate :: Gate (Value Bool) -> Interval Bool
intervalSemGate = \case
  And v1 v2 -> ilift2 (&&) (intervalSem v1) (intervalSem v2)
  Or v1 v2  -> ilift2 (||) (intervalSem v1) (intervalSem v2)
  -- | Since `not` is not a monotonic function, we can not use `ilift` to create
  -- the semantics of `not` on intervals.
  Not v -> let (l, r)   = extremes (intervalSem v)
               (l', r') = (not l, not r)
           in min l' r' ... max l' r'

-- | Analyse a program with interval semantics and compute the width of the interval.
range :: Num r => Value r -> r
range = width . intervalSem

-- | Analyse a program with interval semantics and compute the width of the interval.
rangeQuery :: Num r => Query a r -> r
rangeQuery = width . intervalSemQuery

-- | Given a Query inside the DSL,
-- construct the function from its input `a` to its result `r`
runQuery :: Query a r -> a -> r
runQuery (OneAttr q) = run . bindCase q
runQuery (TwoAttr q) = \(a,b) -> run (bindCase2 q a b)
runQuery (FourAttr q) = \(a,b,c,d) -> run (bindCase4 q a b c d)

-- | Convert a query in our framework to a query on the regular, concrete data
-- type.
bindCase :: Reifyable t os => (PatternsOf t os -> r) -> t -> r
bindCase f = f . reify

-- | Convert a query in our framework to a query on the regular, concrete data
-- type.
bindCase2 :: (Reifyable t1 os1, Reifyable t2 os2) => (PatternsOf t1 os1 -> PatternsOf t2 os2 -> r) -> t1 -> t2 -> r
bindCase2 f t1 t2 = f (reify t1) (reify t2)

-- | Convert a query in our framework to a query on the regular, concrete data
-- type.
bindCase4 :: (Reifyable t1 os1, Reifyable t2 os2, Reifyable t3 os3, Reifyable t4 os4)
          => (PatternsOf t1 os1 -> PatternsOf t2 os2 -> PatternsOf t3 os3 -> PatternsOf t4 os4 -> r) -> t1 -> t2 -> t3 -> t4 -> r
bindCase4 f t1 t2 t3 t4 = f (reify t1) (reify t2) (reify t3) (reify t4)

-- * Deep embedding

-- | Lift a value.
lit :: r -> Value r
lit = Lit

-- | Apply a constrctur to a dependant computation.
eCase :: (All HasBeenMatched os, Ord r, Num r, Reifyable t os)
     => PatternsOf t os -- ^ The matchee
     -> Query t r   -- ^ The pattern match expression
     -> Value r     -- ^ The result of the computation
eCase = ECase

-- | Construct an open query.
oneAttr :: (All HasBeenMatched os, Ord r, Num r, Reifyable t os)
     => (PatternsOf t os -> Value r) -- ^ The pattern match expression
     -> Query t r                -- ^ The result of the computation
oneAttr = OneAttr

twoAttr :: (Num r, Ord r , All HasBeenMatched os1, All HasBeenMatched os2 , Reifyable a' os1, Reifyable b os2)
       => (PatternsOf a' os1 -> PatternsOf b os2 -> Value r)
       -> Query (a', b) r
twoAttr = TwoAttr

fourAttr :: ( Num r, Ord r
          , All HasBeenMatched os1, All HasBeenMatched os2, All HasBeenMatched os3, All HasBeenMatched os4
          , Reifyable a os1, Reifyable b os2 , Reifyable c os3, Reifyable d os4)
       => (PatternsOf a os1 -> PatternsOf b os2 -> PatternsOf c os3 -> PatternsOf d os4 -> Value r)
      -> Query (a, b, c, d) r
fourAttr = FourAttr

-- | A conditional expression in the Value DSL.
qif :: Value Bool -> Value r -> Value r -> Value r
qif = EIf

-- | Equality, EQ
(==:) :: Ord r => Value r -> Value r -> Value Bool
(==:) = ECompare EQ

-- | Inequality, NEQ
(/=:) :: Ord r => Value r -> Value r -> Value Bool
(/=:) v1 v2 = EGate (Not (v1 ==: v2))

-- | Strictly less than, LT
(<:) :: Ord r => Value r -> Value r -> Value Bool
(<:) = ECompare LT

-- | Strictly larger than, LGT
(>:) :: Ord r => Value r -> Value r -> Value Bool
(>:) = ECompare GT

-- | Less-than-or-equal, LEQ
(<=:) :: Ord r => Value r -> Value r -> Value Bool
l <=: r = (l <: r) ||: (l ==: r)

-- | Greater-than-or-equal, GEQ
(>=:) :: Ord r => Value r -> Value r -> Value Bool
(>=:) = flip (<=:)

-- | Logical OR, LOR
(||:) :: Value Bool -> Value Bool -> Value Bool
(||:) v1 v2 = EGate (Or v1 v2)

-- | Logical AND, LAND
(&&:) :: Value Bool -> Value Bool -> Value Bool
(&&:) v1 v2 = EGate (And v1 v2)

-- | Combine two intervals under a binary operator.
-- In other words: lift a binary function to an Interval setting.
--
-- Note that the function `f` has to be monotonic, i.e. it has to preseve the
-- relative order of the infimum and supremum after `f` has been applied.
ilift2 :: Ord c => (a -> b -> c) -> Interval a -> Interval b -> Interval c
ilift2 f i1 i2 = f (inf i1) (inf i2) ... f (sup i1) (sup i2)

-- | Return the infimum (lower bound) and supremum (upper bound) of an interval.
-- More info can be found at [the wikipedia article on extreme points of a
-- convex hull](https://en.wikipedia.org/wiki/Convex_hull#Extreme_points)
extremes :: Interval a -> (a, a)
extremes a = (inf a, sup a)

    




