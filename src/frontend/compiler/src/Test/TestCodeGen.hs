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
codeGeneratorTest =
    let toBool = Right [Instruction opInteger 0 0 ""
                       ,Instruction opGoto    0 1 ""
                       ,Instruction opNoop    0 0 ""
                       ,Instruction opInteger 1 0 ""
                       ,Instruction opNoop    0 1 ""]
     in test [
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
    , "binary expr"  ~: "plus (+)"
                     ~: cExpr (BinExpr Plus (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opAdd 0 0 ""]

    , "binary expr"  ~: "minus (-)"
                     ~: cExpr (BinExpr Minus (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSubtract 0 0 ""]

    , "binary expr"  ~: "multiply (*)"
                     ~: cExpr (BinExpr Multiply (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opMultiply 0 0 ""]

    , "binary expr"  ~: "divide (/)"
                     ~: cExpr (BinExpr Divide (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opDivide 0 0 ""]

    , "binary expr"  ~: "and"
                     ~: cExpr (BinExpr And (Column "c") (Column "d"))
                     ?: cExpr (Column "c")
                     >: Right [Instruction opNot 0 0 ""
                              ,Instruction opIf  0 0 ""]
                     /: cExpr (Column "d")
                     >: Right [Instruction opNot     0 0 ""
                              ,Instruction opIf      0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opNoop    0 1 ""]

    , "binary expr"  ~: "or"
                     ~: cExpr (BinExpr Or (Column "c") (Column "d"))
                     ?: cExpr (Column "c")
                     >: Right [Instruction opIf  0 0 ""]
                     /: cExpr (Column "d")
                     >: Right [Instruction opIf      0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opNoop    0 1 ""]

    , "binary expr"  ~: "Great (>)"
                     ~: cExpr (BinExpr Gt (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opGt 0 0 ""]
                     /: toBool

    , "binary expr"  ~: "less (<)"
                     ~: cExpr (BinExpr Ls (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opLt 0 0 ""]
                     /: toBool

    , "binary expr"  ~: "great or equal (>=)"
                     ~: cExpr (BinExpr GE (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opGe 0 0 ""]
                     /: toBool

    , "binary expr"  ~: "less or equal (<=)"
                     ~: cExpr (BinExpr LE (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opLe 0 0 ""]
                     /: toBool

    , "binary expr"  ~: "equal (=) (==)"
                     ~: cExpr (BinExpr Eq (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opEq 0 0 ""]
                     /: toBool

    , "binary expr"  ~: "not equal (<>) (!=)"
                     ~: cExpr (BinExpr NE (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opNe 0 0 ""]
                     /: toBool
----------------------------------------------------------
    -- TODO LikeExpr
----------------------------------------------------------
    -- TODO FunctionCall
----------------------------------------------------------
    -- TODO IsNull
----------------------------------------------------------
    -- TODO Between
----------------------------------------------------------
    -- TODO InExpr
----------------------------------------------------------
    , "not expr"     ~: ""
                     ~: cExpr (NotExpr $ Column "c")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opNot 0 0 ""]
----------------------------------------------------------
    ]
