module TestCodeGenerator where


import Ast
import Expr
import Parser
import TestUtils
import Instruction
import CodeGeneratorUtils

import Test.HUnit


-- NOTE assume that
--  table xxx has 4 columns: a, b, c, x
--            has 2 indexes: idx_xxx_a: a, idx_xxx_a_b: (a, b)
--  table yyy has 4 columns: a, b, d, y
--            has 2 indexes: idx_yyy_d: d, idx_yyy_a_b: (a, b)

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
                     ~: cExpr (runParser expr "zzz.a")
                     ?: Left "No such column: zzz.a"

    , "table-column" ~: "wrong column name"
                     ~: cExpr (runParser expr "xxx.d")
                     ?: Left "No such column: xxx.d"

    , "table-column" ~: ""
                     ~: cExpr (runParser expr "xxx.b")
                     ?: Right [Instruction opColumn 0 1 ""]

    , "table-column" ~: ""
                     ~: cExpr (runParser expr "yyy.b")
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
                     ~: cExpr (runParser expr "\"a\"")
                     ?: Right [Instruction opString 0 0 "a"]

    , "const value"  ~: "integer"
                     ~: cExpr (runParser expr "123")
                     ?: Right [Instruction opInteger 123 0 ""]

    , "const value"  ~: "double"
                     ~: cExpr (runParser expr "12.3")
                     ?: Right [Instruction opString 0 0 "12.3"]

    , "const value"  ~: "null"
                     ~: cExpr (ConstValue Null)
                     ?: Right [Instruction opNull 0 0 ""]
----------------------------------------------------------
    , "binary expr"  ~: "plus (+)"
                     ~: cExpr (runParser expr "c+d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opAdd 0 0 ""]

    , "binary expr"  ~: "minus (-)"
                     ~: cExpr (runParser expr "c-d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSubtract 0 0 ""]

    , "binary expr"  ~: "multiply (*)"
                     ~: cExpr (runParser expr "c*d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opMultiply 0 0 ""]

    , "binary expr"  ~: "divide (/)"
                     ~: cExpr (runParser expr "c/d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opDivide 0 0 ""]

    , "binary expr"  ~: "and"
                     ~: cExpr (runParser expr "c and d")
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
                     ~: cExpr (runParser expr "c or d")
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
                     ~: cExpr (runParser expr "c > d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGt 0 1 ""]

    , "binary expr"  ~: "less (<)"
                     ~: cExpr (runParser expr "c < d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLt 0 1 ""]

    , "binary expr"  ~: "great or equal (>=)"
                     ~: cExpr (runParser expr "c >= d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGe 0 1 ""]

    , "binary expr"  ~: "less or equal (<=)"
                     ~: cExpr (runParser expr "c <= d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLe 0 1 ""]

    , "binary expr"  ~: "equal (=) (==)"
                     ~: cExpr (runParser expr "c = d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetEq 0 1 ""]

    , "binary expr"  ~: "not equal (<>) (!=)"
                     ~: cExpr (runParser expr "c <> d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetNe 0 1 ""]
----------------------------------------------------------
    , "like expr"    ~: "like"
                     ~: cExpr (runParser expr "c like d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLike 0 1 ""]

    , "like expr"    ~: "notlike"
                     ~: cExpr (runParser expr "c notlike d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetLike 1 1 ""]

    , "like expr"    ~: "glob"
                     ~: cExpr (runParser expr "c glob d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGlob 0 1 ""]

    , "like expr"    ~: "notglob"
                     ~: cExpr (runParser expr "c notglob d")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opSetGlob 1 1 ""]
----------------------------------------------------------
    , "func call"    ~: "no such function"
                     ~: cExpr (runParser expr "asd()")
                     ?: Left "No such function: asd"

    , "func call"    ~: "too few arguments"
                     ~: cExpr (runParser expr "min()")
                     ?: Left "Too few arguments to function: min"

    , "func call"    ~: "too many arguments"
                     ~: cExpr (runParser expr "max(null,null,null)")
                     ?: Left "Too many arguments to function: max"

    , "func call"    ~: ""
                     ~: cExpr (runParser expr "max(c,d)")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opMax 0 0 ""]

    , "func call"    ~: ""
                     ~: cExpr (runParser expr "min(c,d)")
                     ?: cExpr (Column "c") +: cExpr (Column "d")
                     >: Right [Instruction opMin 0 0 ""]

    , "func call"    ~: ""
                     ~: cExpr (runParser expr "substr(c,1,2)")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opInteger 1 0 ""
                              ,Instruction opInteger 2 0 ""
                              ,Instruction opSubstr  0 0 ""]
----------------------------------------------------------
    , "is null"      ~: ""
                     ~: cExpr (runParser expr "c is null")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opSetIsNull 0 1 ""]
    , "not null"     ~: ""
                     ~: cExpr (runParser expr "c not null")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opSetIsNull 0 1 ""
                              ,Instruction opNot       0 0 ""]
----------------------------------------------------------
    , "between"      ~: ""
                     ~: cExpr (runParser expr "x between c and d")
                     ?: cExpr (Column "x")
                     >: Right [Instruction opDup 0 0 ""]
                     /: cExpr (Column "d")
                     >: Right [Instruction opJGt 0 0 ""
                              ,Instruction opDup 0 0 ""]
                     /: cExpr (Column "c")
                     >: Right [Instruction opJLt     0 0 ""
                              ,Instruction opPop     1 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opPop     1 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opNoop    0 1 ""]
----------------------------------------------------------
    , "in expr"      ~: "empty list"
                     ~: cExpr (runParser expr "x in ()")
                     ?: Right [Instruction opInteger 0 0 ""]

    , "in expr"      ~: ""
                     ~: putRes [Instruction opNoop 0 (-1) ""]
                     +: cExpr (runParser expr "x in (1,2)")
                     ?: Right [Instruction opInteger 1 0 ""
                              ,Instruction opSetInsert 0 0 ""
                              ,Instruction opInteger 2 0 ""
                              ,Instruction opSetInsert 0 0 ""
                              ,Instruction opNoop 0 (-1) ""]
                     /: cExpr (Column "x")
                     >: Right [Instruction opSetFound 0 0 ""]
                     /: toBool

    , "in expr"      ~: "right-hand side of IN is not a constant (column)"
                     ~: cExpr (runParser expr "x in (x)")
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (function-call)"
                     ~: cExpr (runParser expr "x in (min(x))")
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (bin-expr)"
                     ~: cExpr (runParser expr "x in (x>1)")
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (like-expr)"
                     ~: cExpr (runParser expr "x in (x like \"a\")")
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (isnull-expr)"
                     ~: cExpr (runParser expr "x in (x is null)")
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (between-expr)"
                     ~: cExpr (runParser expr "x in (x between 1 and 2)")
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (not-expr)"
                     ~: cExpr (runParser expr "x in (not x)")
                     ?: Left "Right-hand side of IN operator must be constant"
----------------------------------------------------------
    , "not expr"     ~: ""
                     ~: cExpr (runParser expr "not c")
                     ?: cExpr (Column "c")
                     >: Right [Instruction opNot 0 0 ""]
----------------------------------------------------------
-- Test code generator: cExprWrapper
----------------------------------------------------------
    , "test"         ~: "" -- use index idx_yyy_d and idx_xxx_a_b, eq-pair xxx.a = 3 fall back to simple expr
                     ~: cExprWrapper (runParser Parser.expr "d = 1 + 2 and 4 = xxx.b  and xxx.a = 3 and yyy.a = 1 and yyy.b > 10")
                     ?: Right []
    ]
