
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module TypeLift.TH.NSPatternSynonyms (genPattern) where

import Language.Haskell.TH
import TypeLift.TH.Util (prepNamesR, getTName)


-- * TH Code generation of Pattern Synonyms

-- | Build signatures and definitions for pattern synonyms
-- Notice length nameinfo + 1
-- Currently the empty list counts as an iteration of the recursive list
-- i.e (x : y : []) has a recursive depth of 3 but the length is 2
--
-- /Example:/ Pattern synonyms
--
-- > data T = T1 | T2 | T3
-- > ... singletons ...
-- > pattern T1_ = Z (I ST1)
-- > pattern T2_ = S (Z (I ST2))
-- > pattern T3_ = S (S (Z (I ST3)))
--
genPattern :: Dec -> Q [Dec]
genPattern d = do
  let nameinfo = prepNamesR d
  let typeName = getTName d
  let len = fromIntegral $ length nameinfo + 1
  defs <- mapM (\(i,suf,o) -> genPatDef suf ("S" ++ o) i) nameinfo

  let nInfo = map (\(_i,_suf,o) -> o) nameinfo
  newSigs <- mapM (\(i,suf,_o) -> genSignatur suf nInfo typeName len i) nameinfo
  return (newSigs ++ defs)


genPatDef :: String  -- ^ Pattern name (Suffixed with "_")
          -> String  -- ^ Singleton name (Prefixed with "S")
          -> Integer -- ^ Index to represent depth of NS
          -> Q Dec
genPatDef p_n s_n i = return $
  PatSynD (mkName p_n) (PrefixPatSyn []) ImplBidir pat
  where
    pat :: Pat
    pat = ConP (mkName "TSnd") [idxRec s_n i]

idxRec :: String -> Integer -> Pat
idxRec _n 1 = ConP (mkName "Z")
              [ConP (mkName "Yes")
                []]

idxRec n l = ConP (mkName "S") [idxRec n (l-1)]


genSignatur :: String   -- ^ Pattern name
            -> [String] -- ^ Explicit names
            -> String   -- ^ Type name
            -> Integer  -- ^ When to stop
            -> Integer  -- ^ Where to be explicit
            -> Q Dec
genSignatur pn ns typeName stop idx = return $
  PatSynSigD (mkName pn)
  (ForallT [] [] (ForallT [] [] typesig))
  where
    typesig = AppT ttupT nsT
    ttupT = AppT (ConT (mkName "TTup")) (ConT (mkName typeName))
    nsT = AppT (AppT
                (ConT (mkName "NS")) (ConT (mkName "Matched")))
               (signatur ns idx 1 stop)

signatur :: [String] -- ^ The type to be put in the singleton
         -> Integer  -- ^ Where to be explicit
         -> Integer  -- ^ Current position
         -> Integer  -- ^ When to stop
         -> Type
signatur [] _ _ _ = PromotedNilT
signatur (_:ns) idx cur stop
  | cur == stop = PromotedNilT
  | cur == idx  = AppT (AppT PromotedConsT
                         (ConT (mkName "()")))
                  (signatur ns idx (cur+1) stop)
  | otherwise = AppT (AppT PromotedConsT
                        (VarT (mkName ("o" ++ show cur))))
                (signatur ns idx (cur+1) stop)
