
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | Some examples with conditional logic

module Conditional where

import DSL.Deep
import TypeLift.Trick (trick)

data D = A | B | C | D deriving (Show)

$(mkTCBoiler ''D)

plessthan1 =
  qif (lit 5 <: lit 10)
  (lit 1)
  (lit 2)

plessthaneq1 = qif (lit 5 <=: lit 10)
               (lit 20)
               (lit 100)

plessthaneq2 d1 = qif (d1 <=: lit 10)
                  (lit 20)
                  (lit 100)

pgreatherthan1 =
  qif (left >: right)
      doLeft
      doRight
    where
      left  = lit 10
      right = lit 20
      doLeft =
        eCase D_ $ query $ \case
          D_ -> lit 25
          C_ -> lit 50
          _  -> lit 10
      doRight =
        eCase A_ $ query $ \case
          A_ -> lit 75
          B_ -> lit 750
          _  -> lit 999

--------------------------
----- Testing new solution with a data type T
data T = T1 | T2 | T3 deriving Show

$(mkTCBoiler ''T)

testProg = eCase T1_ (query pCase)
  where
    pCase = \case T1_ -> lit 10
                  _   -> lit 40

foo = \case
  T1_ -> 10
  T2_ -> 20
  _   -> 30

foo2 = \case
  T1_ -> 10
  _   -> 20


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


data K = K1 | K2 | K3 | K4 | K5 | K6 | K7 | K8 | K9 | K10
  deriving (Bounded, Enum, Show)
$(mkTCBoiler ''K)

someFibs = fib . \case
  K1_ -> 5
  K2_ -> 10
  K3_ -> 4
  K4_ -> 4
  _   -> 30

someFibsNaive = fib . \case
  K1 -> 5
  _  -> 30

naiveTrick :: (K -> r) -> [r]
naiveTrick = map <$> id <*> const [minBound .. maxBound]


kfoo = \case
  K1_ -> 10
  K2_ -> 20
  K5_ -> 50
  _   -> 0

-- | Turn a function f matching on the abstract representaiton of K
-- into a function that matches on the concrete constructors of K.
kify f = f . reify

data ABC = AA | BB | CC

-- mkTCBoiler ''ABC

-- TSnd  :: b -> TTup a b

pattern AA_ :: TTup ABC (NS Match '[(), o2, o3])
pattern AA_ = TSnd (Z Yes)

pattern BB_ :: TTup ABC (NS Match '[o1, (), o3])
pattern BB_ = TSnd (S (Z Yes))

pattern CC_ :: TTup ABC (NS Match '[o1, o2, ()])
pattern CC_ = TSnd (S (S (Z Yes)))

abcfoo = \case
  AA_ -> 10
  _   -> 1337

cabcfoo = \case
  CC_ -> 20
  _   -> 1337

instance  (o1 ~ (), o2 ~ (), o3 ~ ()) => Reifyable ABC [o1, o2, o3] where
  reify = \case
    AA -> AA_
    BB -> BB_
    CC -> CC_
  deify = \case
    AA_ -> AA
    BB_ -> BB
    CC_ -> CC
