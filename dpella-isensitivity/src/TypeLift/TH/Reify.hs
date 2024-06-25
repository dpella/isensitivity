
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module TypeLift.TH.Reify where

import Language.Haskell.TH
import TypeLift.TH.Util (prepNamesO,prepNamesS, getTName, fetchNames)
import Data.Tuple (swap)

genReify :: Dec -> Q [Dec]
genReify d = do
  let reify = mkReify
  let deify = mkDeify
  return $ genInstance d reify deify
  where
    matches :: [(String, String)] -> [Match]
    matches = map genMatch
    cases = zip <$> (map snd . prepNamesO) <*> (map snd . prepNamesS) $ d
    mkReify = mkCase (mkName "reify") (matches cases)
    mkDeify = mkCase (mkName "deify") (matches $ map swap cases)
    mkCase name cases = ValD (VarP name) (NormalB (LamCaseE cases)) []
    genMatch :: (String, String) -> Match
    genMatch (pat, rhs) =
      let pat' = ConP (mkName pat) []
          rhs' = NormalB (ConE (mkName rhs))
      in Match pat' rhs' []

-- * Generate instances of `Reifyable`
--
-- class Reifyable t os where
--  reifGen :: t -> MatchT t os
--  deifGen :: MatchT t os -> t
--
-- i.e. instance (o1 ~ (), o2 ~ (), o3 ~ ()) => Reifyable T [o1,o2,o3] where
genInstance :: Monad m => Dec -> Dec -> Dec -> m Dec
genInstance type' reify deify = return $
  InstanceD incoherent context
  (mkSig (mkName "Reifyable") over) -- I.e. "Reifyable T '[o1, o2, o3]"
  instanceFunctions        -- Class functions.
  where
    incoherent = Nothing
    context = [ tvar ~~ unitT | tvar <- typeVariables ]
    -- The concrete types for which we provide an instance of Reifyable.
    over = [ConT (mkName (getTName type')), mkTypeList typeVariables]
    -- Generate the list of type variables.
    typeVariables = let nConstructors = fromIntegral . length . fetchNames $ type'
                    in reverse [ VarT (mkName ("o" ++ show i)) | i <- [1 .. nConstructors] ]
    instanceFunctions = [reify, deify]

-- | Create a signature
mkSig :: Name -> [Type] -> Type
mkSig tname types =
  let sigType = ConT tname
  in foldl AppT sigType types

-- | I.e. mkTypeEqual "o1" "()" => o1 ~ ()
(~~) :: Type -> Type -> Type
left ~~ right = AppT (AppT EqualityT left) right

-- | Alias for the unit type
unitT :: Type
unitT = ConT (mkName "()")

-- | Join together many types to form a type level list.
mkTypeList :: [Type] -> Type
mkTypeList = foldl joinT PromotedNilT
  where joinT acc nextT = AppT (AppT PromotedConsT nextT) acc
