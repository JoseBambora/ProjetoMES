module RunTest where
import PicoC
import Evaluate
import Exp (Exp(Bool))

runTest :: PicoC -> (Inputs, Int) -> Bool
runTest ast (l, r) = result == r
    where result = evaluate ast l

runTestSuite :: PicoC -> [(Inputs, Int)] -> Bool
runTestSuite ast = all (\(inputs, expected) -> runTest ast (inputs, expected))
