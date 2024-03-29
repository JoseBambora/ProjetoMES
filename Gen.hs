{-# LANGUAGE DeriveDataTypeable #-}
module Gen where 

import Test.QuickCheck
import PicoC
import Parser
import Exp
import Inst

prop_parser :: PicoC -> Bool
prop_parser (PicoC c) = parser (unparser (PicoC c)) == (PicoC c)
