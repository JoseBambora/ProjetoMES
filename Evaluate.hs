module Evaluate where

import PicoC

type Inputs = [(String,Int)]

evaluate :: PicoC -> Inputs -> Int
evaluate _ _ = 0