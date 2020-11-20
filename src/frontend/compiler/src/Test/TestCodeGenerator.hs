module TestCodeGenerator where


import TestUtils
import Instruction
import CodeGeneratorUtils

import Test.HUnit


-- NOTE assume that
--  table xxx has 4 columns: a, b, c, x
--            has 2 indexes: idx_xxx_a  : a,
--                           idx_xxx_a_b: (a, b)
--  table yyy has 4 columns: a, b, d, y
--            has 2 indexes: idx_yyy_d  : d,
--                           idx_yyy_a_b: (a, b)

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
                     ~: cExprStr "zzz.a"
                     ?: Left "No such column: zzz.a"

    , "table-column" ~: "wrong column name"
                     ~: cExprStr "xxx.d"
                     ?: Left "No such column: xxx.d"

    , "table-column" ~: ""
                     ~: cExprStr "xxx.b"
                     ?: Right [Instruction opColumn 0 1 ""]

    , "table-column" ~: ""
                     ~: cExprStr "yyy.b"
                     ?: Right [Instruction opColumn 1 1 ""]
----------------------------------------------------------
    , "column"       ~: "wrong column name"
                     ~: cExprStr "e"
                     ?: Left "No such column: e"

    , "column"       ~: "anbigous column name"
                     ~: cExprStr "a"
                     ?: Left "Ambiguous column name: a"

    , "column"       ~: "anbigous column name"
                     ~: cExprStr "b"
                     ?: Left "Ambiguous column name: b"

    , "column"       ~: ""
                     ~: cExprStr "c"
                     ?: Right [Instruction opColumn 0 2 ""]

    , "column"       ~: ""
                     ~: cExprStr "d"
                     ?: Right [Instruction opColumn 1 2 ""]
----------------------------------------------------------
    , "const value"  ~: "string"
                     ~: cExprStr "\"a\""
                     ?: Right [Instruction opString 0 0 "a"]

    , "const value"  ~: "integer"
                     ~: cExprStr "123"
                     ?: Right [Instruction opInteger 123 0 ""]

    , "const value"  ~: "double"
                     ~: cExprStr "12.3"
                     ?: Right [Instruction opString 0 0 "12.3"]

    , "const value"  ~: "null"
                     ~: cExprStr "null"
                     ?: Right [Instruction opNull 0 0 ""]
----------------------------------------------------------
    , "binary expr"  ~: "plus (+)"
                     ~: cExprStr "c+d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opAdd 0 0 ""]

    , "binary expr"  ~: "minus (-)"
                     ~: cExprStr "c-d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSubtract 0 0 ""]

    , "binary expr"  ~: "multiply (*)"
                     ~: cExprStr "c*d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opMultiply 0 0 ""]

    , "binary expr"  ~: "divide (/)"
                     ~: cExprStr "c/d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opDivide 0 0 ""]

    , "binary expr"  ~: "and"
                     ~: cExprStr "c and d"
                     ?: cExprStr "d"
                     >: Right [Instruction opNot 0 0 ""
                              ,Instruction opJIf 0 0 ""]
                     /: cExprStr "c"
                     >: Right [Instruction opNot     0 0 ""
                              ,Instruction opJIf     0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opNoop    0 1 ""]

    , "binary expr"  ~: "or"
                     ~: cExprStr "c or d"
                     ?: cExprStr "d"
                     >: Right [Instruction opJIf 0 0 ""]
                     /: cExprStr "c"
                     >: Right [Instruction opJIf     0 0 ""
                              ,Instruction opInteger 0 0 ""
                              ,Instruction opGoto    0 1 ""
                              ,Instruction opNoop    0 0 ""
                              ,Instruction opInteger 1 0 ""
                              ,Instruction opNoop    0 1 ""]

    , "binary expr"  ~: "Great (>)"
                     ~: cExprStr "c > d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetGt 0 1 ""]

    , "binary expr"  ~: "less (<)"
                     ~: cExprStr "c < d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetLt 0 1 ""]

    , "binary expr"  ~: "great or equal (>=)"
                     ~: cExprStr "c >= d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetGe 0 1 ""]

    , "binary expr"  ~: "less or equal (<=)"
                     ~: cExprStr "c <= d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetLe 0 1 ""]

    , "binary expr"  ~: "equal (=) (==)"
                     ~: cExprStr "c = d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetEq 0 1 ""]

    , "binary expr"  ~: "not equal (<>) (!=)"
                     ~: cExprStr "c <> d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetNe 0 1 ""]
----------------------------------------------------------
    , "like expr"    ~: "like"
                     ~: cExprStr "c like d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetLike 0 1 ""]

    , "like expr"    ~: "notlike"
                     ~: cExprStr "c notlike d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetLike 1 1 ""]

    , "like expr"    ~: "glob"
                     ~: cExprStr "c glob d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetGlob 0 1 ""]

    , "like expr"    ~: "notglob"
                     ~: cExprStr "c notglob d"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opSetGlob 1 1 ""]
----------------------------------------------------------
    , "func call"    ~: "no such function"
                     ~: cExprStr "asd()"
                     ?: Left "No such function: asd"

    , "func call"    ~: "too few arguments"
                     ~: cExprStr "min()"
                     ?: Left "Too few arguments to function: min"

    , "func call"    ~: "too many arguments"
                     ~: cExprStr "max(null,null,null)"
                     ?: Left "Too many arguments to function: max"

    , "func call"    ~: ""
                     ~: cExprStr "max(c,d)"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opMax 0 0 ""]

    , "func call"    ~: ""
                     ~: cExprStr "min(c,d)"
                     ?: cExprStr "c" +: cExprStr "d"
                     >: Right [Instruction opMin 0 0 ""]

    , "func call"    ~: ""
                     ~: cExprStr "substr(c,1,2)"
                     ?: cExprStr "c"
                     >: Right [Instruction opInteger 1 0 ""
                              ,Instruction opInteger 2 0 ""
                              ,Instruction opSubstr  0 0 ""]
----------------------------------------------------------
    , "is null"      ~: ""
                     ~: cExprStr "c is null"
                     ?: cExprStr "c"
                     >: Right [Instruction opSetIsNull 0 1 ""]
    , "not null"     ~: ""
                     ~: cExprStr "c not null"
                     ?: cExprStr "c"
                     >: Right [Instruction opSetIsNull 0 1 ""
                              ,Instruction opNot       0 0 ""]
----------------------------------------------------------
    , "between"      ~: ""
                     ~: cExprStr "x between c and d"
                     ?: cExprStr "x"
                     >: Right [Instruction opDup 0 0 ""]
                     /: cExprStr "d"
                     >: Right [Instruction opJGt 0 0 ""
                              ,Instruction opDup 0 0 ""]
                     /: cExprStr "c"
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
                     ~: cExprStr "x in ()"
                     ?: Right [Instruction opInteger 0 0 ""]

    , "in expr"      ~: ""
                     ~: putRes [Instruction opNoop 0 (-1) ""]
                     +: cExprStr "x in (1,2)"
                     ?: Right [Instruction opInteger 1 0 ""
                              ,Instruction opSetInsert 0 0 ""
                              ,Instruction opInteger 2 0 ""
                              ,Instruction opSetInsert 0 0 ""
                              ,Instruction opNoop 0 (-1) ""]
                     /: cExprStr "x"
                     >: Right [Instruction opSetFound 0 0 ""]
                     /: toBool

    , "in expr"      ~: "right-hand side of IN is not a constant (column)"
                     ~: cExprStr "x in (x)"
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (function-call)"
                     ~: cExprStr "x in (min(x))"
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (bin-expr)"
                     ~: cExprStr "x in (x>1)"
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (like-expr)"
                     ~: cExprStr "x in (x like \"a\")"
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (isnull-expr)"
                     ~: cExprStr "x in (x is null)"
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (between-expr)"
                     ~: cExprStr "x in (x between 1 and 2)"
                     ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"      ~: "right-hand side of IN is not a constant (not-expr)"
                     ~: cExprStr "x in (not x)"
                     ?: Left "Right-hand side of IN operator must be constant"
----------------------------------------------------------
    , "not expr"     ~: ""
                     ~: cExprStr "not c"
                     ?: cExprStr "c"
                     >: Right [Instruction opNot 0 0 ""]
----------------------------------------------------------
-- Test code generator: cExprWrapper
----------------------------------------------------------
    , "test index"   ~: "" -- use index idx_yyy_d and idx_xxx_a_b, eq-pair xxx.a = 3 fall back to simple expr
                           -- index: (xxx.a, xxx.b) = (3, 4)
                           -- index: yyy.d = 1 + 2
                           -- cond : yyy.a = 1 and yyy.b > 10
                     ~: cExprWrapperStr "d = 1 + 2 and 4 = xxx.b  and xxx.a = 3 and yyy.a = 1 and yyy.b > 10"
                     ?: Right [Instruction opOpen     0 0 "xxx"
                              ,Instruction opOpen     1 0 "yyy"
                              ,Instruction opOpen     2 0 "idx_xxx_a_b"
                              ,Instruction opOpen     3 0 "idx_yyy_d"
                              ,Instruction opInteger  3 0 ""
                              ,Instruction opInteger  4 0 ""
                              ,Instruction opMakeKey  2 0 ""
                              ,Instruction opBeginIdx 2 0 ""
                              ,Instruction opNoop     0 1 ""
                              ,Instruction opNextIdx  2 0 ""
                              ,Instruction opMoveTo   0 0 ""]
                     /: cExprStr "1+2"
                     >: Right [Instruction opMakeKey  1 0 ""
                              ,Instruction opBeginIdx 3 0 ""
                              ,Instruction opNoop     0 2 ""
                              ,Instruction opNextIdx  3 1 ""
                              ,Instruction opMoveTo   1 0 ""]
                     /: (putLabel 3 >> cExprStr "yyy.a = 1 and yyy.b>10")
                     >: Right [Instruction opNot      0 0 ""
                              ,Instruction opJIf      0 2 ""
                              ,Instruction opTempInst 0 0 ""
                              ,Instruction opGoto     0 2 ""
                              ,Instruction opNoop     0 0 ""
                              ,Instruction opClose    0 0 ""
                              ,Instruction opClose    1 0 ""
                              ,Instruction opClose    2 0 ""
                              ,Instruction opClose    3 0 ""]
    , "test index"   ~: "" -- only use index idx_yyy_d
                           -- index: yyy.d = 1
                           -- cond : xxx.b > 10 and yyy.a > 9
                     ~: cExprWrapperStr "xxx.b > 10 and d = 1 and yyy.a > 9"
                     ?: Right [Instruction opOpen     0 0 "xxx"
                              ,Instruction opOpen     1 0 "yyy"
                              ,Instruction opOpen     2 0 "idx_yyy_d"
                              ,Instruction opInteger  1 0 ""
                              ,Instruction opMakeKey  1 0 ""
                              ,Instruction opBeginIdx 2 0 ""
                              ,Instruction opNoop     0 1 ""
                              ,Instruction opNextIdx  2 0 ""
                              ,Instruction opMoveTo   1 0 ""
                              ,Instruction opRewind   0 0 ""
                              ,Instruction opNoop     0 2 ""
                              ,Instruction opNext     0 1 ""]
                     /: (putLabel 3 >> cExprStr "xxx.b > 10 and yyy.a > 9")
                     >: Right [Instruction opNot      0 0 ""
                              ,Instruction opJIf      0 2 ""
                              ,Instruction opTempInst 0 0 ""
                              ,Instruction opGoto     0 2 ""
                              ,Instruction opNoop     0 0 ""
                              ,Instruction opClose    0 0 ""
                              ,Instruction opClose    1 0 ""
                              ,Instruction opClose    2 0 ""]
    ]