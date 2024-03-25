{-# LANGUAGE DeriveDataTypeable #-}
module Gen where 

import Test.QuickCheck
import PicoC



-- parser (unparser ast) == ast

prop_parser :: PicoC -> Property
prop_parser (PicoC c) = collect (length c) $ parser (unparser (PicoC c)) == (PicoC c)
