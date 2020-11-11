module TestUtils where


import Test.HUnit
import FFIStructure
import CodeGeneratorUtils

import Control.Monad.State
import Control.Monad.Except


type CGTestCase = (CodeGenEnv, CodeGenRes)

testEnv :: CodeGenState
testEnv = (
    [ TableMetadata "xxx" [] ["a", "b", "c", "x"] 0
    , TableMetadata "yyy" [] ["a", "b", "d", "y"] 0]
    , []
    , (0, 0))


runCodeGen :: CodeGenEnv -> CodeGenRes
runCodeGen x = evalState (runExceptT x) testEnv


infixl 3 +:
(+:) :: CodeGenEnv -> CodeGenEnv -> CodeGenEnv
a +: b = a >> b


infixr 2 >:
(>:) :: CodeGenEnv -> CodeGenRes -> CodeGenRes
x >: y =
    case evalState (runExceptT x) testEnv of
         Right res  -> case y of
            Right res' -> Right $ res ++ res'
            _          -> y
         l@(Left _) -> l


infixl 1 /:
(/:) :: CodeGenRes -> CodeGenRes -> CodeGenRes
a /: b = case (a, b) of
    (Left _, _) -> a
    (_, Left _) -> b
    (Right x, Right y) -> Right $ x ++ y


infixr 0 ?:
(?:) :: CodeGenEnv -> CodeGenRes -> Test
a ?: b = runCodeGen a ~?= b
