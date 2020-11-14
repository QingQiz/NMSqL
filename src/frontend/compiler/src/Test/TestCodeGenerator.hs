module TestCodeGenerator where


import Ast
import Expr
import TestUtils
import Instruction
import CodeGeneratorUtils

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
-- Test code generator: expr
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
                              ,Instruction opJIf 0 0 ""]
                     /: cExpr (Column "d")
                     >: Right [Instruction opNot     0 0 ""
                              ,Instruction opJIf     0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opNoop    0 1 ""]

    , "binary expr"  ~: "or"
                     ~: cExpr (BinExpr Or (Column "c") (Column "d"))
                     ?: cExpr (Column "c")
                     >: Right [Instruction opJIf 0 0 ""]
                     /: cExpr (Column "d")
                     >: Right [Instruction opJIf     0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opNoop    0 1 ""]

    , "binary expr"  ~: "Great (>)"
                     ~: cExpr (BinExpr Gt (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGt 0 1 ""]

    , "binary expr"  ~: "less (<)"
                     ~: cExpr (BinExpr Ls (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLt 0 1 ""]

    , "binary expr"  ~: "great or equal (>=)"
                     ~: cExpr (BinExpr GE (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGe 0 1 ""]

    , "binary expr"  ~: "less or equal (<=)"
                     ~: cExpr (BinExpr LE (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLe 0 1 ""]

    , "binary expr"  ~: "equal (=) (==)"
                     ~: cExpr (BinExpr Eq (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetEq 0 1 ""]

    , "binary expr"  ~: "not equal (<>) (!=)"
                     ~: cExpr (BinExpr NE (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetNe 0 1 ""]
----------------------------------------------------------
    , "like expr"    ~: "like"
                     ~: cExpr (LikeExpr Like (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLike 0 1 ""]

    , "like expr"    ~: "notlike"
                     ~: cExpr (LikeExpr NotLike (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLike 1 1 ""]

    , "like expr"    ~: "glob"
                     ~: cExpr (LikeExpr Glob (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGlob 0 1 ""]

    , "like expr"    ~: "notglob"
                     ~: cExpr (LikeExpr NotGlob (Column "c") (Column "d"))
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGlob 1 1 ""]
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
                     >: Right [Instruction opSetIsNull 0 1 ""]
----------------------------------------------------------
    , "between"      ~: ""
                     ~: cExpr (Between (Column "x") (Column "c") (Column "d"))
                     ?: cExpr (Column "x")
                     >: Right [Instruction opDup 0 0 ""]
                     /: cExpr (Column "d")
                     >: Right [Instruction opJGe 0 0 ""]
                     /: cExpr (Column "c")
                     >: Right [Instruction opJLe     0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opNoop    0 1 ""]
----------------------------------------------------------
    , "in expr"      ~: "empty list"
                     ~: cExpr (InExpr (Column "x") $ ValueList [])
                     ?: Right [Instruction opInteger 0 0 ""]

    , "in expr"      ~: ""
                     ~: putRes [Instruction opNoop 0 (-1) ""]
                     +: cExpr (InExpr (Column "x") $ ValueList $ map (ConstValue . ValInt) [1, 2])
                     ?: Right [Instruction opInteger 1 0 ""
                              ,Instruction opSetInsert 0 0 ""
                              ,Instruction opInteger 2 0 ""
                              ,Instruction opSetInsert 0 0 ""
                              ,Instruction opNoop 0 (-1) ""]
                     /: cExpr (Column "x")
                     >: Right [Instruction opSetFound 0 0 ""]
                     /: toBool

    , "in expr"      ~: "right-hand side of IN is not a constant (column)"
                     ~: cExpr (InExpr (Column "x") $ ValueList [Column "x"])
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (function-call)"
                     ~: cExpr (InExpr (Column "x") $ ValueList [FunctionCall "min" [Column "x"]])
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (bin-expr)"
                     ~: cExpr (InExpr (Column "x") $ ValueList [BinExpr Gt (Column "x") (ConstValue Null)])
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (like-expr)"
                     ~: cExpr (InExpr (Column "x") $ ValueList [LikeExpr Like (Column "x") (ConstValue Null)])
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (isnull-expr)"
                     ~: cExpr (InExpr (Column "x") $ ValueList [IsNull (Column "x")])
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (between-expr)"
                     ~: cExpr (InExpr (Column "x") $ ValueList [Between (Column "x") (ConstValue Null) (ConstValue Null)])
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (not-expr)"
                     ~: cExpr (InExpr (Column "x") $ ValueList [NotExpr (Column "x")])
                     ?: Left "Right-hand side of IN operator must be constant"
----------------------------------------------------------
    , "not expr"     ~: ""
                     ~: cExpr (NotExpr $ Column "c")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opNot 0 0 ""]
----------------------------------------------------------
    ]
