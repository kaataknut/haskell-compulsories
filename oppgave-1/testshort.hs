module Test where
import Oblig12016
import Oblig12016Test.hs

testE :: IO()
testE = runTestTT testCbrActualE && runTestTT testCbrActualAstE