module RunTest where

import PicoC
import Evaluate
import Exp
import Inst
import Mutations

programa1 = PicoC [(IFE (Var "a") [(Print (Add (Const 2) (Const 3))),(Return (Var "a"))] [(Return (Const 30))])]
programa2 = PicoC [(IFE (Const 0) [(Return (Bool False))] [(Return (Bool True))])]
programa3 = PicoC [(IFE (Not (Const 0)) [(Return (Bool False))] [(Return (Bool True))])]
programa4 = PicoC [Atrib "" "b" (Add (Var "b") (Const 2)), Return (Var "b")]
programa5 = PicoC [(Dec "int" "c"), Atrib "" "c" (Const 2), Atrib "int" "d" (Const 3) , (While (Var "a") [Atrib "" "b" (Add (Var "b") (Const 2)), Atrib "" "a" (Sub (Var "a") (Const 1))]), Return (Var "b")]

runEvaluate :: PicoC -> Inputs -> (Int,[String])
runEvaluate ast l = (result,insts)
    where 
        (result,insts,prints) = evaluate ast l

runTest :: PicoC -> (Inputs, Int) -> Bool
runTest ast (l, r) = fst (runEvaluate ast l) == r

runTestSuite :: PicoC -> [(Inputs, Int)] -> Bool
runTestSuite ast = all (\(inputs, expected) -> runTest ast (inputs, expected))

runTestSuiteMutation :: PicoC -> [(Inputs, Int)] -> Bool
runTestSuiteMutation ast = all (\(inputs, expected) -> runTest (mutationPicoC ast) (inputs, expected))

runTestInsts :: PicoC -> (Inputs, Int) -> [String]
runTestInsts ast (l, r)  = snd (runEvaluate ast l)

runTestSuiteInsts :: PicoC -> [(Inputs, Int)] -> [[String]]
runTestSuiteInsts ast = map (\(inputs, expected) -> runTestInsts ast (inputs, expected))

input1 :: Inputs
input1 = [("a",4),("b",6)]

input2 :: Inputs
input2 = [("a",5),("b",10)]

input3 :: Inputs
input3 = [("a",2),("b",1)]

input4 :: Inputs
input4 = [("a",0),("b",1)]

input5 :: Inputs
input5 = [("a",2),("b",50)]


testeRun1 = runTestSuite programa5 [(input1, 14), (input2, 20)]
testeRun2 = runTestSuite programa4 [(input1, 8) , (input2, 12)]
testeRun3 = runTestSuite programa1 [(input1, 4) , (input2, 5)]
testeRun4 = runTestSuite programa2 [(input1, 1) , (input2, 1)]
testeRun5 = runTestSuiteInsts programa5 [(input1, 14), (input2, 20)]
testeRun6 = runTestSuiteInsts programa4 [(input1, 8) , (input2, 12)]
testeRun7 = runTestSuiteInsts programa1 [(input1, 4) , (input2, 5)]
testeRun8 = runTestSuiteInsts programa2 [(input1, 1) , (input2, 1)]

testeRun9  = runTestSuiteMutation programa5 [(input1, 14), (input2, 20)]
testeRun10 = runTestSuiteMutation programa4 [(input1, 8) , (input2, 12)]
testeRun11 = runTestSuiteMutation programa1 [(input1, 4) , (input2, 5)]
testeRun12 = runTestSuiteMutation programa2 [(input1, 1) , (input2, 1)]

testeRun13 = runTestSuiteMutation programa1 [(input3, 2)]

testeRunAll = testeRun1 && testeRun2 && testeRun3 && testeRun4
testeRunAllMutation = testeRun9 && testeRun10 && testeRun11 && testeRun12



testPrint :: PicoC -> Inputs -> IO ()
testPrint ast l = do putStrLn printLine
    where 
        (result,insts,prints) = evaluate ast l
        p = (foldr (\x acc -> x ++ "\n" ++ acc) "" prints)
        printLine = take (length p - 1) p
