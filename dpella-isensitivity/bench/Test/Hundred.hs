
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# OPTIONS_GHC -O0 #-}

module Test.Hundred where

import TH.DataType
import DSL.Deep

$(mkTestData "Test100" 100)
$(mkTCBoiler ''Test100)
