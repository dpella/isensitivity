
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module TypeLift.Trick where

import Data.SOP
    ( All,
      NS(..),
      HPure(hcpure),
      NP(..),
      Proxy(..))
import Cmm (Width(W256))

-- * Data types

-- | A data type for signifying if we matched on a case or not.
data Matched a = Yes | No
  deriving Show

-- | TypVar describes the relation between type variables in the user defined
-- function. Namely, a lifted type is always accompanied by either a type
-- variable, or the same type explicitly matched.
-- a : The type
-- b : type variable, or Witness if explicitly matched.
data TTup t idx where
  TSnd :: idx -> TTup t idx

-- * Type classes

-- | Represents a witness to some type t.
-- Explicitly instanced types get the witness in terms of
-- the Yes constructor, otherwise there is No witness.
class HasBeenMatched t where
  answer :: Matched t

instance HasBeenMatched () where
  answer = Yes

instance {-# INCOHERENT #-} HasBeenMatched a where
  answer = No

-- * Functions

-- | Apply the Evidence Proxy to all types
-- contained in ev.
defined :: forall ts . All HasBeenMatched ts => NP Matched ts
defined = hcpure (Proxy :: Proxy HasBeenMatched) answer

-- | Recurse an NP of Maybe, if we find a Just,
-- its inhabitated by an explicit type and we
-- give it an index. Ignore and keep recurse if
-- Nothing.
indexes :: forall ts . NP Matched ts -> [NS Matched ts]
indexes Nil         = []
indexes (No  :* rs) = map S (indexes rs)
indexes (Yes :* rs) = Z Yes : map S (indexes rs)

-- | Find the first occurence of a non explicit match in the NP,
-- and convert it to a Universed value.
addWild :: forall ts. All HasBeenMatched ts => NP Matched ts -> NP Matched ts
addWild Nil       = Nil
addWild (No :* r) = Yes :* r
addWild (p  :* r) = p :* addWild r

-- | The new version of the trick
-- A function containing a case expression is represented as a function
-- from an NS to some value r. Collect all witnessed constructors, represented
-- as an NS. Apply all the witnessed constructors to the case expression.
trick :: forall ts r t . (All HasBeenMatched ts)
      => (PatternsOf t ts -> r) -- ^ Concrete case expression
      -> [r]                   -- ^ Possible outputs
trick f = map f' matches
  where matches = indexes (addWild (defined @ts))
        f'      = f . TSnd

-- * TODO

class Reifyable t ts where
  reify :: t -> PatternsOf t ts
  deify :: PatternsOf t ts -> t

npString :: All Show ts => NP Maybe ts -> [String]
npString Nil = []
npString (Nothing :* np) = npString np
npString (Just v  :* np) = show v : npString np

npExample1, npExample2 :: NP Maybe [Int,Char]
npExample1 = Just 42 :* Just 'a' :* Nil 
npExample2 = Nothing :* Just 'b' :* Nil 

tt = npString npExample2
-- >>> npString npExample2
-- ["'b'"]

class SomeValue a where
  values :: [a]

instance SomeValue Int where
  values = [42]

instance SomeValue Char where
  values = ['x','y']

ttt :: All SomeValue ts => NP [] ts 
ttt = m (Proxy :: Proxy SomeValue) values

-- >>> :t sample
-- sample :: Sample a => [a]

-- >>> ttt @[Int,Int,Char]
-- [42] :* [42] :* "xy" :* Nil

-- >>> :t hcpure 
-- hcpure
--   :: (AllN h c xs, HPure h) =>
--      proxy c -> (forall a. c a => f a) -> h f xs

m :: forall c ts f . All c ts => Proxy c -> (forall a. c a => f a) -> NP f ts 
m = hcpure  


t = answer @()
 
-- >>> t 
-- Yes

-- t' :: forall x.  Proxy x -> Matched x
-- t' p = answer @ x

-- >>> answer @()
-- Yes


t' :: forall x. Matched x 
t' = answer @x   
-- No

-- >>> indexes test
-- >>> test
-- [Z Yes,S (S (Z Yes))]
-- Yes :* No :* Yes :* No :* Nil

type PatternsOf t ts = TTup t (NS Matched ts)

data T = A | B | C 

pattern A_ :: PatternsOf T '[(), o2, o3]
pattern A_ = TSnd (Z Yes)

pattern B_ :: PatternsOf T '[o1, (), o3]
pattern B_ = TSnd (S (Z Yes))

pattern C_ :: PatternsOf T '[o1, o2, ()]
pattern C_ = TSnd (S (S (Z Yes)))

{------------------------- 
   Demo IFL 
--------------------------}

foo A_ = 10 
foo _  = 1 

whichones :: NP Matched [(), t2, ()] 
whichones = defined 

-- >>> whichones
-- Yes :* No :* Yes :* Yes :* No :* No :* Nil

idxs = indexes whichones 

-- >>> idxs 
-- [Z Yes,S (S (Z Yes))]


-- >>> addWild whichones

-- >>> trick foo
-- [10,1,5]
