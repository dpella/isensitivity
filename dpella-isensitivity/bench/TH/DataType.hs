
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | Template Haskell code for generating large data types to be used in
-- benchmarking.

module TH.DataType where

import Language.Haskell.TH

mkTestData :: (Num a, Enum a, Show a) => String -> a -> Q [Dec]
mkTestData name n =
  pure $ sumTypeBuilder (mkName name) [mkName (name <> show c) | c <- [1..n]]

-- | Create a
sumTypeBuilder :: Name -> [Name] -> [Dec]
sumTypeBuilder name constructors =
  mkData name [mkConst c | c <- constructors]
  where
    mkData n consts =
      [DataD [] n [] Nothing
       consts
       []
      ]
    mkConst c = NormalC c []
