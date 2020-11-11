module TestCodeGen where


import Ast
import Expr
import TestUtils
import Instruction

import Test.HUnit


-- NOTE assume that
--  table xxx has 3 columns: a, b, c, x
--  table yyy has 3 columns: a, b, d, y

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

    , "const value"  ~: "null"
                     ~: cExpr (ConstValue Null)
                     ?: Right [Instruction opNull 0 0 ""]
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
    , "like expr"    ~: "like"
                     ~: cExpr (LikeExpr Like (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opLike 0 0 ""]
                     /: toBool

    , "like expr"    ~: "notlike"
                     ~: cExpr (LikeExpr NotLike (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opLike 1 0 ""]
                     /: toBool

    , "like expr"    ~: "glob"
                     ~: cExpr (LikeExpr Glob (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opGlob 0 0 ""]
                     /: toBool

    , "like expr"    ~: "notglob"
                     ~: cExpr (LikeExpr NotGlob (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opGlob 1 0 ""]
                     /: toBool
----------------------------------------------------------
    , "func call"    ~: "no such function"
                     ~: cExpr (FunctionCall "asd" [])
                     ?: Left "No such function: asd"

    , "func call"    ~: "too few arguments"
                     ~: cExpr (FunctionCall "min" [])
                     ?: Left "Too few arguments to function: min"

    , "func call"    ~: "too many arguments"
                     ~: cExpr (FunctionCall "max" [ConstValue Null, ConstValue Null, ConstValue Null])
                     ?: Left "Too many arguments to function: max"

    , "func call"    ~: ""
                     ~: cExpr (FunctionCall "max" [Column "c", Column "d"])
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opMax 0 0 ""]

    , "func call"    ~: ""
                     ~: cExpr (FunctionCall "min" [Column "c", Column "d"])
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opMin 0 0 ""]
----------------------------------------------------------
    , "is null"      ~: ""
                     ~: cExpr (IsNull $ Column "c")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opIsNull 0 0 ""]
                     /: toBool
----------------------------------------------------------
    , "between"      ~: ""
                     ~: cExpr (Between (Column "x") (Column "c") (Column "d"))
                     ?: cExpr (Column "d") +: cExpr (Column "x")
                     >: Right [Instruction opLe  0 0 ""
                              ,Instruction opIf  0 0 ""
                              ,Instruction opDup 0 0 ""]
                     /: cExpr (Column "c")
                     >: Right [Instruction opLe      0 0 ""
                              ,Instruction opIf      0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opNoop    0 1 ""]
----------------------------------------------------------
    -- TODO InExpr
----------------------------------------------------------
    , "not expr"     ~: ""
                     ~: cExpr (NotExpr $ Column "c")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opNot 0 0 ""]
----------------------------------------------------------
    ]
