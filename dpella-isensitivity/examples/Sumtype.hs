
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Sumtype (D) where

import DSL.Deep

-- * A small module containing some example uses of DSL with interval semantics.

-- | A simple sum type
data D = A | B | C | D deriving (Show)

-- Initializing another boilerplate generator is pretty much
-- the only difference between this module and the other example
-- module.
$(mkTCBoiler ''D)

data Potato = P | O | T deriving (Show, Bounded, Enum)

-- Note: Always infer the type.

pCaseOf1 = \case
  A_ -> 10
  D_ -> 20
  _  -> 50

pCaseOf2 i = eCase i $ lit . \case
  B_ -> 1
  C_ -> 2
  _  -> 5

pCaseOf3 i = eCase i $ lit . \case
                D_ -> 1
                B_ -> (-1)
                _ -> 0

pCaseOf4 i = eCase i $ lit .
  \case
    D_ -> 1
    C_ -> 2
    _ -> 0

pCaseOf5 i = eCase i $ lit . \case
                D_ -> 0.7
                B_ -> 0.5
                _ -> 0

-- In more complex examples, the user might want to create query programs in some fashion
-- close to the following:
-- Define some data to analyze (which could be sequenced programs itself?)
-- Define some check on that data, namely the case
-- create a query program from it
pCaseOf6 = eCase userData userCase
  where userData = A_
        userCase = lit . \case
          A_ -> 10
          B_ -> 20
          _  -> 30
