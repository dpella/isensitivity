
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE ExplicitNamespaces #-}
module TypeLift.TH.STGen (mkTCBoiler, SOP.I (..), SOP.NS (..)) where

import Language.Haskell.TH
import qualified TypeLift.TH.NSPatternSynonyms as NS (genPattern)
import qualified TypeLift.TH.Reify as R (genReify)
import Data.SOP as SOP ( I(..), NS(..) )

-- | Given a Name, reify its information, send the name and the info into
-- a template haskell pipeline for generating Instances and Patterns.
mkTCBoiler :: Name -> Q [Dec]
mkTCBoiler n = reify n >>= \case
  TyConI d -> tcBuilder d
  _        -> error "No TH Data representation in mkTCBoiler"

-- | TH pipeline
tcBuilder :: Dec -> Q [Dec]
tcBuilder d = do
  pats   <- NS.genPattern d
  reifyf <- R.genReify d
  return $ reifyf ++ pats
