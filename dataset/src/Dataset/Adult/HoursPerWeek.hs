
{- 
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

module Dataset.Adult.HoursPerWeek where

import Prelude (Eq, Ord, Enum, Bounded, Show)

data HoursPerWeek
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Eleven
  | Twelve
  | Thirteen
  | Fourteen
  | Fifteen
  | Sixteen
  | Seventeen
  | Eighteen
  | Nineteen
  | Twenty
  | TwentyOne
  | TwentyTwo
  | TwentyThree
  | TwentyFour
  | TwentyFive
  | TwentySix
  | TwentySeven
  | TwentyEight
  | TwentyNine
  | Thirty
  | ThirtyOne
  | ThirtyTwo
  | ThirtyThree
  | ThirtyFour
  | ThirtyFive
  | ThirtySix
  | ThirtySeven
  | ThirtyEight
  | ThirtyNine
  | Forty
  | FortyOne
  | FortyTwo
  | FortyThree
  | FortyFour
  | FortyFive
  | FortySix
  | FortySeven
  | FortyEight
  | FortyNine
  | Fifty
  | FiftyOne
  | FiftyTwo
  | FiftyThree
  | FiftyFour
  | FiftyFive
  | FiftySix
  | FiftySeven
  | FiftyEight
  | FiftyNine
  | Sixty
  | SixtyOne
  | SixtyTwo
  | SixtyThree
  | SixtyFour
  | SixtyFive
  | SixtySix
  | SixtySeven
  | SixtyEight
  | SixtyNine
  | Seventy
  | SeventyOne
  | SeventyTwo
  | SeventyThree
  | SeventyFour
  | SeventyFive
  | SeventySix
  | SeventySeven
  | SeventyEight
  | SeventyNine
  | Eighty
  | EightyOne
  | EightyTwo
  | EightyThree
  | EightyFour
  | EightyFive
  | EightySix
  | EightySeven
  | EightyEight
  | EightyNine
  | Ninety
  | NinetyOne
  | NinetyTwo
  | NinetyThree
  | NinetyFour
  | NinetyFive
  | NinetySix
  | NinetySeven
  | NinetyEight
  | NinetyNine
  deriving (Eq, Ord, Enum, Bounded, Show)
