module TestUtils where


import Test.HUnit
import FFIStructure
import CodeGeneratorUtils

import Control.Monad.State
import Control.Monad.Except

testEnv :: CodeGenState
testEnv = (
    [ TableMetadata "xxx" [] ["a", "b", "c"] 0
    , TableMetadata "yyy" [] ["a", "b", "d"] 0]
    , []
    , 0)


runCodeGen :: CodeGenEnv -> CodeGenRes
runCodeGen x = evalState (runExceptT x) testEnv

connectTestCase :: (CodeGenEnv, CodeGenRes) -> Test
connectTestCase (a, b) = b ~=? runCodeGen a
