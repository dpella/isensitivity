
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Main where

import Nations
import TypeLift.Trick

legalDrinking = tell . \case
        Denmark_ -> (Denmark, 16)
        n        -> (Sweden,  18)
        --n        -> (unReifyNation n, 100)
 where 
  tell (nation, age) = "The legal drinking age in " ++ show nation ++ " is " ++ show age

main :: IO ()
main = do
  mapM_ print (trick legalDrinking)
