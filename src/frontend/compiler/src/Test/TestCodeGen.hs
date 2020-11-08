module TestCodeGen where


import Ast
import Expr
import TestUtils
import Instruction
import CodeGeneratorUtils

import Test.HUnit


----------------------------------------------------------
-- Test code generator for expr
----------------------------------------------------------

cgTestCases :: [(CodeGenEnv, CodeGenRes)]
cgTestCases = [
        (cExpr (TableColumn "zzz" "a"), Left "No such column: zzz.a"),
        (cExpr (TableColumn "xxx" "d"), Left "No such column: xxx.d"),
        (cExpr (TableColumn "yyy" "b"), Right [Instruction opColumn 1 1 ""])
    ]


cgTests :: Test
cgTests = test [
        "test1" ~: "table-column: wrong table name"  ~: connectTestCase $ cgTestCases !! 0,
        "test1" ~: "table-column: wrong column name" ~: connectTestCase $ cgTestCases !! 1,
        "test1" ~: "table-column: right case"        ~: connectTestCase $ cgTestCases !! 2
    ]
