module TestCodeGen where


import Ast
import Expr
import TestUtils
import Instruction

import Test.HUnit


-- NOTE assume that
--  table xxx has 3 columns: a, b, c
--  table yyy has 3 columns: a, b, d

codeGeneratorTest :: Test
codeGeneratorTest = test [
----------------------------------------------------------
-- Test code generator for expr
----------------------------------------------------------
      "table-column" ~: "wrong table name"
                     ~: cExpr (TableColumn "zzz" "a")
                     ?: Left "No such column: zzz.a"

    , "table-column" ~: "wrong column name"
                     ~: cExpr (TableColumn "xxx" "d")
                     ?: Left "No such column: xxx.d"

    , "table-column" ~: ""
                     ~: cExpr (TableColumn "xxx" "b")
                     ?: Right [Instruction opColumn 0 1 ""]

    , "table-column" ~: ""
                     ~: cExpr (TableColumn "yyy" "b")
                     ?: Right [Instruction opColumn 1 1 ""]
----------------------------------------------------------
    , "column"       ~: "wrong column name"
                     ~: cExpr (Column "e")
                     ?: Left "No such column: e"

    , "column"       ~: "anbigous column name"
                     ~: cExpr (Column "a")
                     ?: Left "Ambiguous column name: a"

    , "column"       ~: "anbigous column name"
                     ~: cExpr (Column "b")
                     ?: Left "Ambiguous column name: b"

    , "column"       ~: ""
                     ~: cExpr (Column "c")
                     ?: Right [Instruction opColumn 0 2 ""]

    , "column"       ~: ""
                     ~: cExpr (Column "d")
                     ?: Right [Instruction opColumn 1 2 ""]
----------------------------------------------------------
    , "const value"  ~: "string"
                     ~: cExpr (ConstValue $ ValStr "a")
                     ?: Right [Instruction opString 0 0 "a"]

    , "const value"  ~: "integer"
                     ~: cExpr (ConstValue $ ValInt 123)
                     ?: Right [Instruction opInteger 123 0 ""]

    , "const value"  ~: "double"
                     ~: cExpr (ConstValue $ ValDouble 12.3)
                     ?: Right [Instruction opString 0 0 "12.3"]
----------------------------------------------------------
    -- TODO BinExpr
----------------------------------------------------------
    -- TODO LikeExpr
----------------------------------------------------------
    -- TODO FunctionCall
----------------------------------------------------------
    -- TODO IsNull
----------------------------------------------------------
    -- TODO Between
----------------------------------------------------------
    -- TODO NotExpr
----------------------------------------------------------
    -- TODO InExpr
----------------------------------------------------------
    ]
