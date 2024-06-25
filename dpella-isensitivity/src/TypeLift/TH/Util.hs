
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

-- | Test
module TypeLift.TH.Util
  ( prepNamesR
  , prepNamesS
  , prepNamesO
  , fetchNames
  , getTName
  , getSingName
  ) where

import Language.Haskell.TH
import Data.Generics.Uniplate.Data (universeBi)

-- * Types
type Index = Integer
type SName = String -- ^ (SuffixedName)
type OName = String -- ^ (OriginalName)

-- | Some information about constructors in a data type.
type NameInfo = ( Integer -- ^ A fresh index tied to a constructor
                , SName   -- ^ The constructor name, suffixed with "_", viable for patterns
                , OName   -- ^ The original constructor name.
                )

-- * Function

-- | Given a TH Dec, construct NameInfo triplets for each constructor in the
-- represented data type.
-- \Example:\
--
-- > data T = T1 | T2 | T3
--
-- Applying its Dec representation would yield:
--
-- > [(1,"T1_","T1"),(2,"T2_","T2"),(3,"T3_","T3")]
prepNamesR :: Dec -> [NameInfo]
prepNamesR = zip3 <$> const [1..] <*> map (++ "_") . fetchNames <*> fetchNames

-- | Fetch names and save only index and new suffixed name.
prepNamesS :: Dec -> [(Index,SName)]
prepNamesS = map (\(x,y,_) -> (x, y)) . prepNamesR

-- | Fetch names and save only index and original name.
prepNamesO :: Dec -> [(Index,OName)]
prepNamesO = map (\(x,_,z) -> (x, z)) . prepNamesR

-- | Given a representation of a data type, fetch the names of
-- the constructors
fetchNames :: Dec -> [String]
fetchNames (DataD _ _ _ _ cs _) = [nameBase c | (NormalC c _) <- universeBi cs]
fetchNames _ = error " fetchNames : not a data type abstraction"

-- | Get the name of a type
getTName :: Dec -> String
getTName d = case d of
  (DataD _ n _ _ _ _) -> nameBase n
  _ -> error "getTName : input is not a datatype"

-- | Get the name of a type with singletonPrefix "S"
getSingName :: Dec -> [Char]
getSingName = ("S"++) . getTName
