module TestUtils where


import Parser
import TableMetadata
import CodeGenerator
import CodeGeneratorUtils

import Generator.Expr

import Test.HUnit
import Control.Applicative
import Control.Monad.State
import Control.Monad.Except



type CGTestCase = (CodeGenEnv, CodeGenRes)

testEnv :: CodeGenState
testEnv = (
    ([TableMetadata "xxx" [("idx_xxx_a", ["a"]), ("idx_xxx_a_b", ["a", "b"])] ["a", "b", "c", "x"] 234
     ,TableMetadata "yyy" [("idx_yyy_d", ["d"]), ("idx_yyy_a_b", ["a", "b"])] ["a", "b", "d", "y"] 234]
    , [("max", 2), ("min", 2), ("substr", 3)])
    , ([], [], 0)
    , (CodeGenCnt 0 0 0 0 [] False))


runCodeGen :: CodeGenEnv -> CodeGenRes
runCodeGen x = evalState (runExceptT x) testEnv

cExprStr :: String -> CodeGenEnv
cExprStr s = cExpr $ runParser expr s

cExprWrapperStr :: String -> CodeGenEnv
cExprWrapperStr s = cExprWrapper $ runParser expr s

cSelectStr :: String -> SelectResultType -> CodeGenEnv
cSelectStr s = cSelectWrapper (runParser select s)

cTableActionStr :: String -> CodeGenEnv
cTableActionStr s = cTableActionWrapper (runParser (createTable <|> dropTable) s)

cIndexActionStr :: String -> CodeGenEnv
cIndexActionStr s = cIndexActionWrapper (runParser (createIndex <|> dropIndex) s)

cDeleteStr :: String -> CodeGenEnv
cDeleteStr s = cDeleteWrapper (runParser delete s)

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
