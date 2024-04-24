module RunTest where

import PicoC
import Evaluate
import Exp
import Inst

programa1 = PicoC [(IFE (Const 1) [(Return (Var "a"))] [(Return (Const 30))])]
programa2 = PicoC [(IFE (Const 0) [(Return (Bool False))] [(Return (Bool True))])]
programa3 = PicoC [(IFE (Not (Const 0)) [(Return (Bool False))] [(Return (Bool True))])]
programa4 = PicoC [Atrib "" "b" (Add (Var "b") (Const 2)), Return (Var "b")]
programa5 = PicoC [(Dec "int" "c"), Atrib "" "c" (Const 2), Atrib "int" "d" (Const 3) , (While (Var "a") [Atrib "" "b" (Add (Var "b") (Const 2)), Atrib "" "a" (Sub (Var "a") (Const 1))]), Return (Var "b")]


runTest :: PicoC -> (Inputs, Int) -> Bool
runTest ast (l, r) = result == r
    where (result,insts) = evaluate ast l

runTestSuite :: PicoC -> [(Inputs, Int)] -> Bool
runTestSuite ast = all (\(inputs, expected) -> runTest ast (inputs, expected))

input1 :: Inputs
input1 = [("a",4),("b",6)]

input2 :: Inputs
input2 = [("a",5),("b",10)]

testeRun1 = runTestSuite programa5 [(input1, 14), (input2, 20)]
testeRun2 = runTestSuite programa4 [(input1, 8) , (input2, 12)]

runtestinsts :: PicoC -> (Inputs, Int) -> [String]
runtestinsts ast (l, r)  = insts
    where (result,insts) = evaluate ast l

testeRun3 = runtestinsts programa5 (input1, 14)
testeRun4 = runtestinsts programa4 (input1, 8)
testeRun5 = runtestinsts programa1 (input1, 8)  
testeRun6 = runtestinsts programa2 (input1, 8)  
