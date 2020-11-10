module TestUtils where


import Test.HUnit
import FFIStructure
import CodeGeneratorUtils

import Control.Monad.State
import Control.Monad.Except


type CGTestCase = (CodeGenEnv, CodeGenRes)

testEnv :: CodeGenState
testEnv = (
    [ TableMetadata "xxx" [] ["a", "b", "c"] 0
    , TableMetadata "yyy" [] ["a", "b", "d"] 0]
    , []
    , (0, 0))


runCodeGen :: CodeGenEnv -> CodeGenRes
runCodeGen x = evalState (runExceptT x) testEnv


infixl 1 +:
(+:) :: CodeGenEnv -> CodeGenEnv -> CodeGenEnv
a +: b = a >> b


infixr 0 >:
(>:) :: CodeGenEnv -> CodeGenRes -> CodeGenRes
x >: y =
    case evalState (runExceptT x) testEnv of
         Right res  -> case y of
            Right res' -> Right $ res ++ res'
            _          -> y
         l@(Left _) -> l


infixr 0 ?:
(?:) :: CodeGenEnv -> CodeGenRes -> Test
a ?: b = runCodeGen a ~?= b
