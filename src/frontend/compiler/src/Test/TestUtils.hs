module TestUtils where

import Parser
import Instruction
import FFIStructure
import CodeGeneratorUtils

import Test.HUnit
import Control.Monad.State
import Control.Monad.Except


type CGTestCase = (CodeGenEnv, CodeGenRes)

testEnv :: CodeGenState
testEnv = (
    ([TableMetadata "xxx" [("idx_xxx_a", ["a"]), ("idx_xxx_a_b", ["a", "b"])] ["a", "b", "c", "x"] 0
     ,TableMetadata "yyy" [("idx_yyy_d", ["d"]), ("idx_yyy_a_b", ["a", "b"])] ["a", "b", "d", "y"] 0]
    ,[("max", 2, Just opMax), ("min", 2, Just opMin), ("substr", 3, Just opSubstr)])
    , []
    , (0, 0, 0))


runCodeGen :: CodeGenEnv -> CodeGenRes
runCodeGen x = evalState (runExceptT x) testEnv

runParser :: Parser a -> String -> a
runParser p s = case parse p s of
    Just (a, _) -> a
    _           -> error $ "parse error: " ++ s


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
