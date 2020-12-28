module TestCodeGenerator where


import Ast
import TestUtils
import Instruction
import CodeGenerator
import CodeGeneratorUtils

import Generator.Expr (cExpr)

import Test.HUnit


-- NOTE assume that
--  table xxx has 4 columns: a, b, c, x
--            has 2 indexes: idx_xxx_a  : a,
--                           idx_xxx_a_b: (a, b)
--  table yyy has 4 columns: a, b, d, y
--            has 2 indexes: idx_yyy_d  : d,
--                           idx_yyy_a_b: (a, b)
-- assume that: database cookie is 234

codeGeneratorTest :: Test
codeGeneratorTest = test [
----------------------------------------------------------
-- Test code generator: expr
----------------------------------------------------------
      "tb column"   ~: "wrong table name"
                    ~: cExprStr "zzz.a"
                    ?: Left "No such column: zzz.a"

    , "tb column"   ~: "wrong column name"
                    ~: cExprStr "xxx.d"
                    ?: Left "No such column: xxx.d"

    , "tb column"   ~: ""
                    ~: cExprStr "xxx.b"
                    ?: Right [Instruction opColumn 0 1 ""]

    , "tb column"   ~: ""
                    ~: cExprStr "yyy.b"
                    ?: Right [Instruction opColumn 1 1 ""]
----------------------------------------------------------
    , "column"      ~: "wrong column name"
                    ~: cExprStr "e"
                    ?: Left "No such column: e"

    , "column"      ~: "anbigous column name"
                    ~: cExprStr "a"
                    ?: Left "Ambiguous column name: a"

    , "column"      ~: "anbigous column name"
                    ~: cExprStr "b"
                    ?: Left "Ambiguous column name: b"

    , "column"      ~: ""
                    ~: cExprStr "c"
                    ?: Right [Instruction opColumn 0 2 ""]

    , "column"      ~: ""
                    ~: cExprStr "d"
                    ?: Right [Instruction opColumn 1 2 ""]
---------------------------------------------------------
    , "const value" ~: "string"
                    ~: cExprStr "\"a\""
                    ?: Right [Instruction opString 0 0 "a"]

    , "const value" ~: "integer"
                    ~: cExprStr "123"
                    ?: Right [Instruction opInteger 123 0 ""]

    , "const value" ~: "double"
                    ~: cExprStr "12.3"
                    ?: Right [Instruction opString 0 0 "12.3"]

    , "const value" ~: "null"
                    ~: cExprStr "null"
                    ?: Right [Instruction opNull 0 0 ""]
----------------------------------------------------------
    , "any-column"  ~: "*"
                    ~: cExprStr "max(*,2)"
                    ?: Left "`*' was not allowed here"
----------------------------------------------------------
    , "empty-expr"  ~: ""
                    ~: cExpr EmptyExpr
                    ?: Right [Instruction opInteger 1 0 ""]
----------------------------------------------------------
    , "binary expr" ~: "plus (+)"
                    ~: cExprStr "c+d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opAdd 0 0 ""]

    , "binary expr" ~: "minus (-)"
                    ~: cExprStr "c-d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSubtract 0 0 ""]

    , "binary expr" ~: "multiply (*)"
                    ~: cExprStr "c*d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opMultiply 0 0 ""]

    , "binary expr" ~: "divide (/)"
                    ~: cExprStr "c/d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opDivide 0 0 ""]

    , "binary expr" ~: "and"
                    ~: cExprStr "c and d"
                    ?: cExprStr "c"
                    >: Right [Instruction opJIf     1 0 ""]
                    /: cExprStr "d"
                    >: Right [Instruction opJIf     1 0 ""
                             ,Instruction opInteger 1 0 ""
                             ,Instruction opGoto    0 1 ""
                             ,Instruction opNoop    0 0 ""
                             ,Instruction opInteger 0 0 ""
                             ,Instruction opNoop    0 1 ""]

    , "binary expr" ~: "or"
                    ~: cExprStr "c or d"
                    ?: cExprStr "c"
                    >: Right [Instruction opJIf     0 0 ""]
                    /: cExprStr "d"
                    >: Right [Instruction opJIf     0 0 ""
                             ,Instruction opInteger 0 0 ""
                             ,Instruction opGoto    0 1 ""
                             ,Instruction opNoop    0 0 ""
                             ,Instruction opInteger 1 0 ""
                             ,Instruction opNoop    0 1 ""]

    , "binary expr" ~: "Great (>)"
                    ~: cExprStr "c > d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetGt 0 1 ""]

    , "binary expr" ~: "less (<)"
                    ~: cExprStr "c < d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetLt 0 1 ""]

    , "binary expr" ~: "great or equal (>=)"
                    ~: cExprStr "c >= d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetGe 0 1 ""]

    , "binary expr" ~: "less or equal (<=)"
                    ~: cExprStr "c <= d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetLe 0 1 ""]

    , "binary expr" ~: "equal (=) (==)"
                    ~: cExprStr "c = d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetEq 0 1 ""]

    , "binary expr" ~: "not equal (<>) (!=)"
                    ~: cExprStr "c <> d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetNe 0 1 ""]
----------------------------------------------------------
    , "like expr"   ~: "like"
                    ~: cExprStr "c like d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetLike 0 1 ""]

    , "like expr"   ~: "notlike"
                    ~: cExprStr "c notlike d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetLike 1 1 ""]

    , "like expr"   ~: "glob"
                    ~: cExprStr "c glob d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetGlob 0 1 ""]

    , "like expr"   ~: "notglob"
                    ~: cExprStr "c notglob d"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opSetGlob 1 1 ""]
----------------------------------------------------------
    , "func call"   ~: "no such function"
                    ~: cExprStr "asd()"
                    ?: Left "No such function: asd"

    , "func call"   ~: "too few arguments"
                    ~: cExprStr "min(a)"
                    ?: Left "Too few arguments to function: min"

    , "func call"   ~: "too many arguments"
                    ~: cExprStr "max(null,null,null)"
                    ?: Left "Too many arguments to function: max"

    , "func call"   ~: ""
                    ~: cExprStr "max(c,d)"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opMax 0 0 ""]

    , "func call"   ~: ""
                    ~: cExprStr "min(c,d)"
                    ?: cExprStr "c" +: cExprStr "d"
                    >: Right [Instruction opMin 0 0 ""]

    , "func call"   ~: ""
                    ~: cExprStr "substr(c,1,2)"
                    ?: cExprStr "c"
                    >: Right [Instruction opInteger 1 0 ""
                             ,Instruction opInteger 2 0 ""
                             ,Instruction opSubstr  0 0 ""]
----------------------------------------------------------
    , "is null"     ~: ""
                    ~: cExprStr "c is null"
                    ?: cExprStr "c"
                    >: Right [Instruction opSetIsNull 0 1 ""]
    , "not null"    ~: ""
                    ~: cExprStr "c not null"
                    ?: cExprStr "c"
                    >: Right [Instruction opSetIsNull 0 1 ""
                             ,Instruction opNot       0 0 ""]
----------------------------------------------------------
    , "between"     ~: ""
                    ~: cExprStr "x between c and d"
                    ?: cExprStr "x"
                    >: Right [Instruction opDup     0 0 ""]
                    /: cExprStr "d"
                    >: Right [Instruction opJGt     0 0 ""
                             ,Instruction opDup     0 0 ""]
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
    , "in expr"     ~: "empty list"
                    ~: cExprStr "x in ()"
                    ?: Right [Instruction opInteger 0 0 ""]

    , "in expr"     ~: ""
                    ~: putRes [Instruction opNoop     0 (-1) ""]
                    +: cExprStr "x in (1,2)"
                    ?: Right [Instruction opSetOpen   0 0 ""
                             ,Instruction opInteger   1 0 ""
                             ,Instruction opSetInsert 0 0 ""
                             ,Instruction opInteger   2 0 ""
                             ,Instruction opSetInsert 0 0 ""
                             ,Instruction opNoop      0 (-1) ""]
                    /: cExprStr "x"
                    >: Right [Instruction opSetSetFound 0 1 ""]

    , "in expr"     ~: "right-hand side of IN is not a constant (column)"
                    ~: cExprStr "x in (x)"
                    ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"     ~: "right-hand side of IN is not a constant (function-call)"
                    ~: cExprStr "x in (min(x))"
                    ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"     ~: "right-hand side of IN is not a constant (bin-expr)"
                    ~: cExprStr "x in (x>1)"
                    ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"     ~: "right-hand side of IN is not a constant (like-expr)"
                    ~: cExprStr "x in (x like \"a\")"
                    ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"     ~: "right-hand side of IN is not a constant (isnull-expr)"
                    ~: cExprStr "x in (x is null)"
                    ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"     ~: "right-hand side of IN is not a constant (between-expr)"
                    ~: cExprStr "x in (x between 1 and 2)"
                    ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"     ~: "right-hand side of IN is not a constant (not-expr)"
                    ~: cExprStr "x in (not x)"
                    ?: Left "Right-hand side of IN operator must be constant"

    , "in expr"     ~: "in select"
                    ~: cExprStr "x in (select * from xxx)"
                    ?: Left "Only a single result allowed for a SELECT that is part of an expression"

    , "in expr"     ~: "in select"
                    ~: cExprStr "a in (select a from xxx)"
                    ?: Left "Ambiguous column name: a"

    , "in expr"     ~: "in (select a from xxx)"
                    ~: putRes [Instruction opNoop 0 (-1) ""]
                    +: cExprStr "xxx.a in (select a from yyy)"
                    ?: Right [Instruction opSetOpen 0 0 ""]
                    /: (getMetadata >>= \mds -> putMetadata [last mds])
                    +: cExprWrapper EmptyExpr
                    +: insertTemp (appendInstructions
                        [Instruction opColumn 0 0 ""
                        ,Instruction opSetInsert 0 0 ""])
                    +: appendInstructions
                        [Instruction opNoop 0 (-1) ""
                        ,Instruction opColumn 0 0 ""
                        ,Instruction opSetSetFound 0 1 ""]
                    +: removeTemp
                    >: Right []
---------------------------------------------------------
    , "not expr"    ~: ""
                    ~: cExprStr "not c"
                    ?: cExprStr "c"
                    >: Right [Instruction opNot 0 0 ""]
----------------------------------------------------------
-- Test code generator: cExprWrapper
----------------------------------------------------------
-- XXX  expr order can be optimized
{-

    rewind "xxx"                                      rewind "xxx"
    next   "xxx"                                      next   "xxx"
    rewind "yyy"                        -->           expr "xxx.b > 10"
    next   "yyy"                                      rewind "yyy"
    expr "yyy.a > 9 and xxx.b > 10"                   next   "yyy"
                                                      expr "yyy.a > 9"
-}
    , "test index"  ~: "" -- index: (xxx.a, xxx.b) = (3, 4)
                          -- index: yyy.d = 1 + 2
                          -- cond : yyy.a = 1 and yyy.b > 10
                    ~: cExprWrapperStr "d = 1 + 2 and 4 = xxx.b  and xxx.a = 3 and yyy.a = 1 and yyy.b > 10"
                    ?: Right [Instruction opOpen     0 0 "idx_xxx_a_b"
                             ,Instruction opOpen     1 0 "idx_yyy_d"
                             ,Instruction opVerifyCookie 234 0 ""
                             ,Instruction opInteger  3 0 ""
                             ,Instruction opInteger  4 0 ""
                             ,Instruction opMakeKey  2 0 ""
                             ,Instruction opBeginIdx 0 0 ""
                             ,Instruction opNoop     0 1 ""]
                    /: cExprStr "1+2"
                    >: Right [Instruction opMakeKey  1 0 ""
                             ,Instruction opBeginIdx 1 0 ""
                             ,Instruction opNoop     0 3 ""]
                    /: (putLabel 5 >> cExprStr "yyy.b>10 and yyy.a = 1")
                    >: Right [Instruction opJIf      1 4 ""
                             ,Instruction opTempInst 0 0 ""
                             ,Instruction opNoop     0 4 ""
                             ,Instruction opNextIdx  1 2 ""
                             ,Instruction opGoto     0 3 ""
                             ,Instruction opNoop     0 2 ""
                             ,Instruction opNextIdx  0 0 ""
                             ,Instruction opGoto     0 1 ""
                             ,Instruction opNoop     0 0 ""
                             ,Instruction opClose    0 0 ""
                             ,Instruction opClose    1 0 ""]
    , "test index"  ~: "" -- index: yyy.d = 1
                          -- cond : xxx.b > 10 and yyy.a > 9
                    ~: cExprWrapperStr "xxx.b > 10 and d = 1 and yyy.a > 9"
                    ?: Right [Instruction opOpen     0 0 "xxx"
                             ,Instruction opOpen     1 0 "idx_yyy_d"
                             ,Instruction opVerifyCookie 234 0 ""
                             ,Instruction opInteger  1 0 ""
                             ,Instruction opMakeKey  1 0 ""
                             ,Instruction opBeginIdx 1 0 ""
                             ,Instruction opNoop     0 1 ""
                             ,Instruction opRewind   0 0 ""
                             ,Instruction opNoop     0 3 ""]
                    /: (putLabel 5 >> cExprStr "xxx.b > 10 and yyy.a > 9")
                    >: Right [Instruction opJIf      1 4 ""
                             ,Instruction opTempInst 0 0 ""
                             ,Instruction opNoop     0 4 ""
                             ,Instruction opNext     0 2 ""
                             ,Instruction opGoto     0 3 ""
                             ,Instruction opNoop     0 2 ""
                             ,Instruction opNextIdx  1 0 ""
                             ,Instruction opGoto     0 1 ""
                             ,Instruction opNoop     0 0 ""
                             ,Instruction opClose    0 0 ""
                             ,Instruction opClose    1 0 ""]
    , "test index"  ~: "no index"
                    ~: cExprWrapperStr "xxx.b > 10 and yyy.a > 9"
                    ?: Right [Instruction opOpen     0 0 "xxx"
                             ,Instruction opOpen     1 0 "yyy"
                             ,Instruction opVerifyCookie 234 0 ""
                             ,Instruction opRewind   0 0 ""
                             ,Instruction opNoop     0 1 ""
                             ,Instruction opRewind   1 0 ""
                             ,Instruction opNoop     0 3 ""]
                    /: (putLabel 5 >> cExprStr "xxx.b > 10 and yyy.a > 9")
                    >: Right [Instruction opJIf      1 4 ""
                             ,Instruction opTempInst 0 0 ""
                             ,Instruction opNoop     0 4 ""
                             ,Instruction opNext     1 2 ""
                             ,Instruction opGoto     0 3 ""
                             ,Instruction opNoop     0 2 ""
                             ,Instruction opNext     0 0 ""
                             ,Instruction opGoto     0 1 ""
                             ,Instruction opNoop     0 0 ""
                             ,Instruction opClose    0 0 ""
                             ,Instruction opClose    1 0 ""]
----------------------------------------------------------
-- Test code generator: cSelect
----------------------------------------------------------
-- NOTE we should test help functions first
    , "insert-temp" ~: ""
                    ~: putRes [Instruction opNoop     0 0 ""
                              ,Instruction opTempInst 0 0 ""
                              ,Instruction opNoop     0 1 ""]
                    +: insertTemp (appendInst opNoop  0 2 "")
                    ?: Right  [Instruction opNoop     0 0 ""
                              ,Instruction opNoop     0 2 ""
                              ,Instruction opTempInst 0 0 ""
                              ,Instruction opNoop     0 1 ""]
    , "prependEnv"  ~: ""
                    ~: putRes [Instruction opNoop    0 0 ""]
                    +: prependEnv (appendInst opNoop 0 1 "")
                    ?: Right  [Instruction opNoop    0 1 ""
                              ,Instruction opNoop    0 0 ""]
----------------------------------------------------------
    , "sel-res"     ~: "select *,a from xxx"
                    ~: cSelectStr "select *,a from xxx" Normal
                    ?: Left "Semantic error on `*'"
    , "sel-res"     ~: "select * form xxx -- (to set)"
                    ~: cSelectStr "select * from xxx" (ToSet 0)
                    ?: Left "Only a single result allowed for a SELECT that is part of an expression"
    , "sel-res"     ~: "union select a from xxx -- (need 2 result)"
                    ~: cSelectStr "select a from xxx" (UnionSel Union 2)
                    ?: Left "SELECTs to the left and right of UNION do not have the same number of result columns"
----------------------------------------------------------
    , "select"      ~: "select * from xxx"
                    ~: cSelectStr "select * from xxx" Normal
                    ?: (getMetadata >>= \mds -> putMetadata [head mds]) -- only use metadata of table xxx
                    +: cExprWrapper EmptyExpr
                    +: insertTemp (appendInstructions
                        [Instruction opColumn       0   0 ""
                        ,Instruction opColumn       0   1 ""
                        ,Instruction opColumn       0   2 ""
                        ,Instruction opColumn       0   3 ""
                        ,Instruction opCallback     4   0 ""])
                    +: prependEnv (appendInstructions
                        [Instruction opColumnCount  4   0 ""
                        ,Instruction opColumnName   0   0 "xxx.a"
                        ,Instruction opColumnName   1   0 "xxx.b"
                        ,Instruction opColumnName   2   0 "xxx.c"
                        ,Instruction opColumnName   3   0 "xxx.x"])
                    +: removeTemp
                    >: Right []
    , "select"      ~: "select count(*) from xxx where a > 1"
                    ~: cSelectStr "select count(*) from xxx where a > 1" Normal
                    ?: (getMetadata >>= \mds -> putMetadata [head mds]) -- only use metadata of table xxx
                    +: cExprWrapperStr "a>1"
                    +: insertTemp (appendInstructions
                        [Instruction opAggIncr 1 0 ""])
                    +: prependEnv (appendInstructions
                        [Instruction opColumnCount  1   0 ""
                        ,Instruction opColumnName   0   0 "count(*)"
                        ,Instruction opAggReset     0   1 ""])
                    +: appendInstructions
                        [Instruction opAggGet   0 0 ""
                        ,Instruction opCallback 1 0 ""]
                    +: removeTemp
                    >: Right []
    , "select"      ~: "select max(max(a,b)) from xxx"
                    ~: cSelectStr "select max(max(a,b)) from xxx" Normal
                    ?: (getMetadata >>= \mds -> putMetadata [head mds]) -- only use metadata of table xxx
                    +: cExprWrapper EmptyExpr
                    +: insertTemp (cExprStr "max(a,b)")
                    +: insertTemp (appendInstructions
                        [Instruction opAggGet 0 0 ""
                        ,Instruction opMax    0 0 ""
                        ,Instruction opAggSet 0 0 ""])
                    +: prependEnv (appendInstructions
                        [Instruction opColumnCount  1   0 ""
                        ,Instruction opColumnName   0   0 "max(max(a,b))"
                        ,Instruction opAggReset     0   1 ""])
                    +: appendInstructions
                        [Instruction opAggGet   0 0 ""
                        ,Instruction opCallback 1 0 ""]
                    +: removeTemp
                    >: Right []
    , "select"      ~: "select max(max(a),min(b)) from xxx"
                    ~: cSelectStr "select max(max(a),min(b)) from xxx" Normal
                    ?: (getMetadata >>= \mds -> putMetadata [head mds]) -- only use metadata of table xxx
                    +: cExprWrapper EmptyExpr
                    +: insertTemp (appendInstructions
                        [Instruction opColumn 0 0 ""
                        ,Instruction opAggGet 0 0 ""
                        ,Instruction opMax    0 0 ""
                        ,Instruction opAggSet 0 0 ""
                        ,Instruction opColumn 0 1 ""
                        ,Instruction opAggGet 0 1 ""
                        ,Instruction opMin    0 0 ""
                        ,Instruction opAggSet 0 1 ""])
                    +: prependEnv (appendInstructions
                        [Instruction opColumnCount  1   0 ""
                        ,Instruction opColumnName   0   0 "max(max(a),min(b))"
                        ,Instruction opAggReset     0   2 ""])
                    +: appendInstructions
                        [Instruction opAggGet   0 0 ""
                        ,Instruction opAggGet   0 1 ""
                        ,Instruction opMax      0 0 ""
                        ,Instruction opCallback 1 0 ""]
                    +: removeTemp
                    >: Right []
    , "select"      ~: "select a,max(a) from xxx"
                    ~: cSelectStr "select a,max(a) from xxx" Normal
                    ?: (getMetadata >>= \mds -> putMetadata [head mds]) -- only use metadata of table xxx
                    +: cExprWrapper EmptyExpr
                    +: insertTemp (appendInstructions
                        [Instruction opColumn 0 0 ""
                        ,Instruction opAggSet 0 0 ""
                        ,Instruction opColumn 0 0 ""
                        ,Instruction opAggGet 0 1 ""
                        ,Instruction opMax    0 0 ""
                        ,Instruction opAggSet 0 1 ""])
                    +: prependEnv (appendInstructions
                        [Instruction opColumnCount  2   0 ""
                        ,Instruction opColumnName   0   0 "a"
                        ,Instruction opColumnName   1   0 "max(a)"
                        ,Instruction opAggReset     0   2 ""])
                    +: appendInstructions
                        [Instruction opAggGet   0 0 ""
                        ,Instruction opAggGet   0 1 ""
                        ,Instruction opCallback 2 0 ""]
                    +: removeTemp
                    >: Right []
----------------------------------------------------------
-- Test code generator: create table / drop table
----------------------------------------------------------
    , "create table"~: "create table xxx (a int)"
                    ~: cTableActionStr "create table xxx (a int)"
                    ?: Left "table xxx already exists"

    , "create table"~: "create table zzz(a int unique primary key check(a>1), b string(3) unique, primary key (b))"
                    ~: cTableActionStr "create table zzz(a int unique primary key check(a>1), b string(3), primary key (b))"
                    ?: let idx :: Integer -> [Instruction]
                           idx n = [Instruction opDefaultKey  0 0 ""
                                   ,Instruction opString      0 0 "index"
                                   ,Instruction opString      0 0 $ "(zzz autoindex " ++ show n ++ ")"
                                   ,Instruction opString      0 0 "zzz"
                                   ,Instruction opCreateIndex 0 0 ""
                                   ,Instruction opNull        0 0 ""
                                   ,Instruction opMakeRecord  5 0 ""
                                   ,Instruction opPut         0 0 ""]
                        in Right $ [Instruction opTransaction  0   0 ""
                                   ,Instruction opVerifyCookie 234 0 ""
                                   ,Instruction opOpenWrite    0   0 "NMSqL_Master"]
                                ++ idx 1 ++ idx 2 ++ idx 3
                                ++ [Instruction opDefaultKey   0          0 ""
                                   ,Instruction opString       0          0 "table"
                                   ,Instruction opString       0          0 "zzz"
                                   ,Instruction opString       0          0 "zzz"
                                   ,Instruction opCreateTable  0          0 ""
                                   ,Instruction opString       0          0 "CREATE TABLE zzz (a int UNIQUE PRIMARY KEY ASC CHECK((a>1)),b string(3),PRIMARY KEY(b))"
                                   ,Instruction opMakeRecord   5          0 ""
                                   ,Instruction opPut          0          0 ""
                                   ,Instruction opSetCookie    1725595867 0 ""
                                   ,Instruction opClose        0          0 ""
                                   ,Instruction opCommit       0          0 ""]

    , "drop table"  ~: "drop table zzz"
                    ~: cTableActionStr "drop table zzz"
                    ?: Left "no such table: zzz"

    , "drop table"  ~: "drop table xxx"
                    ~: cTableActionStr "drop table xxx"
                    ?: Right [Instruction opTransaction  0          0 ""
                             ,Instruction opVerifyCookie 234        0 ""
                             ,Instruction opOpenWrite    0          0 "NMSqL_Master"
                             ,Instruction opRewind       0          0 ""
                             ,Instruction opNoop         0          1 ""
                             ,Instruction opString       0          0 "xxx"
                             ,Instruction opColumn       0          2 ""
                             ,Instruction opJNe          0          2 ""
                             ,Instruction opColumn       0          3 ""
                             ,Instruction opDelete       0          0 ""
                             ,Instruction opDestroy      0          0 ""
                             ,Instruction opNoop         0          2 ""
                             ,Instruction opNext         0          0 ""
                             ,Instruction opGoto         0          1 ""
                             ,Instruction opNoop         0          0 ""
                             ,Instruction opSetCookie    1725595867 0 ""
                             ,Instruction opClose        0          0 ""
                             ,Instruction opCommit       0          0 ""]
----------------------------------------------------------
-- Test code generator: create index / drop index
----------------------------------------------------------
    , "create index"~: "create index idx_xxx_a on xxx(a)"
                    ~: cIndexActionStr "create index idx_xxx_a on xxx (a)"
                    ?: Left "index idx_xxx_a already exists"

    , "create index"~: "create index idx_xxx_z on xxx(z)"
                    ~: cIndexActionStr "create index idx_xxx_z on xxx (z)"
                    ?: Left "no such column: z"

    , "create index"~: "create index idx_xxx_a_x on xxx(a,x)"
                    ~: cIndexActionStr "create index idx_xxx_a_x on xxx(a,x)"
                    ?: Right [Instruction opTransaction  0   0 ""
                             ,Instruction opVerifyCookie 234 0 ""
                             -- write index to master table
                             ,Instruction opOpenWrite    0          0 "NMSqL_Master"
                             ,Instruction opDefaultKey   0          0 ""
                             ,Instruction opString       0          0 "index"
                             ,Instruction opString       0          0 "idx_xxx_a_x"
                             ,Instruction opString       0          0 "xxx"
                             ,Instruction opCreateIndex  0          0 ""
                             ,Instruction opString       0          0 "CREATE INDEX idx_xxx_a_x ON xxx(a,x)"
                             ,Instruction opMakeRecord   5          0 ""
                             ,Instruction opPut          0          0 ""
                             ,Instruction opClose        0          0 ""
                             -- update data
                             ,Instruction opOpen         0          0 "xxx"
                             ,Instruction opOpenWrite    1          0 "idx_xxx_a_x"
                             ,Instruction opRewind       0          0 ""
                             ,Instruction opNoop         0          1 ""
                             -- index key
                             ,Instruction opColumn       0          0 ""
                             ,Instruction opColumn       0          3 ""
                             ,Instruction opMakeKey      2          0 ""
                             -- index value
                             ,Instruction opAddress      0          0 ""
                             ,Instruction opMakeRecord   1          0 ""
                             -- update index
                             ,Instruction opPut          1          0 ""
                             ,Instruction opNoop         0          2 ""
                             ,Instruction opNext         0          0 ""
                             ,Instruction opGoto         0          1 ""
                             ,Instruction opNoop         0          0 ""
                             ,Instruction opSetCookie    1725595867 0 ""
                             ,Instruction opClose        0          0 ""
                             ,Instruction opClose        1          0 ""
                             ,Instruction opCommit       0          0 ""]

    , "drop index"  ~: "drop index idx_xxx_z"
                    ~: cIndexActionStr "drop index idx_xxx_z"
                    ?: Left "no such index: idx_xxx_z"

    , "drop index"  ~: "drop index idx_xxx_a"
                    ~: cIndexActionStr "drop index idx_xxx_a"
                    ?: Right [Instruction opTransaction  0          0 ""
                             ,Instruction opVerifyCookie 234        0 ""
                             ,Instruction opOpenWrite    0          0 "NMSqL_Master"
                             ,Instruction opRewind       0          0 ""
                             ,Instruction opNoop         0          1 ""
                             ,Instruction opString       0          0 "idx_xxx_a"
                             ,Instruction opColumn       0          1 ""
                             ,Instruction opJNe          0          2 ""
                             ,Instruction opColumn       0          3 ""
                             ,Instruction opDelete       0          0 ""
                             ,Instruction opDestroy      0          0 ""
                             ,Instruction opGoto         0          0 ""
                             ,Instruction opNoop         0          2 ""
                             ,Instruction opNext         0          0 ""
                             ,Instruction opGoto         0          1 ""
                             ,Instruction opNoop         0          0 ""
                             ,Instruction opSetCookie    1725595867 0 ""
                             ,Instruction opClose        0          0 ""
                             ,Instruction opCommit       0          0 ""]
----------------------------------------------------------
-- Test code generator: delete from
----------------------------------------------------------
    , "delete"  ~: "delete from asd"
                ~: cDeleteStr "delete from asd"
                ?: Left "no such table: asd"

    , "delete"  ~: "delete from xxx"
                ~: cDeleteStr "delete from xxx"
                ?: Right [Instruction opTransaction  0   0 ""
                         ,Instruction opVerifyCookie 234 0 ""
                         ,Instruction opOpen         0   0 "NMSqL_Master"
                         ,Instruction opRewind       0   0 ""
                         ,Instruction opNoop         0   1 ""
                         ,Instruction opString       0   0 "xxx"
                         ,Instruction opColumn       0   2 ""
                         ,Instruction opJNe          0   2 ""
                         ,Instruction opColumn       0   3 ""
                         ,Instruction opClear        0   0 ""
                         ,Instruction opNoop         0   2 ""
                         ,Instruction opNext         0   0 ""
                         ,Instruction opGoto         0   1 ""
                         ,Instruction opNoop         0   0 ""
                         ,Instruction opClose        0   0 ""
                         ,Instruction opCommit       0   0 ""]

    , "delete"  ~: "delete from xxx where a = 1"
                ~: cDeleteStr "delete from xxx where a = 1"
                ?: Right
                    [Instruction opTransaction  0   0 ""
                    ,Instruction opVerifyCookie 234 0 ""]
                /: (getMetadata >>= \mds -> putMetadata [head mds] >> putWriteFlag True)
                -- after do this, label should be 3
                +: cExprWrapperStr "a=1"
                -- delete data from idx_xxx_a_b
                -- after do this, label should be 6
                +: insertTemp (appendInstructions
                    [Instruction opOpenWrite    1 0 "idx_xxx_a_b"
                    -- make key (a, b) for index_xxx_a_b
                    ,Instruction opColumn       0 0 ""
                    ,Instruction opColumn       0 1 ""
                    ,Instruction opMakeKey      2 0 ""
                    -- begin index
                    ,Instruction opBeginIdx     1 0 ""
                    ,Instruction opNoop         0 4 ""
                    -- compare address, jump when negative
                    ,Instruction opAddress      0 0 ""
                    ,Instruction opAddress      1 0 ""
                    ,Instruction opJNe          0 5 ""
                    -- delete and break
                    ,Instruction opDelete       1 0 ""
                    ,Instruction opGoto         0 3 ""
                    --
                    ,Instruction opNoop         0 5 ""
                    ,Instruction opNextIdx      0 3 ""
                    ,Instruction opGoto         0 4 ""
                    ,Instruction opNoop         0 3 ""
                    --
                    ,Instruction opClose        1 0 ""])
                -- delte from table
                +: insertTemp (appendInstructions
                    [Instruction opOpenWrite    1 0 "xxx"
                    -- rewind
                    ,Instruction opRewind       1 0 ""
                    ,Instruction opNoop         0 7 ""
                    -- compare address, jump when negative
                    ,Instruction opAddress      0 0 ""
                    ,Instruction opAddress      1 0 ""
                    ,Instruction opJNe          0 8 ""
                    -- delete and break
                    ,Instruction opDelete       1 0 ""
                    ,Instruction opGoto         0 6 ""
                    --
                    ,Instruction opNoop         0 8 ""
                    ,Instruction opNext         0 6 ""
                    ,Instruction opGoto         0 7 ""
                    ,Instruction opNoop         0 6 ""
                    --
                    ,Instruction opClose        1 0 ""])
                -- delete from index_xxx_a
                +: insertTemp (appendInst opDelete 0 0 "")
                +: removeTemp
                >: Right [Instruction opCommit 0 0 ""]

    , "delete"  ~: "delete from xxx where b = 1"
                ~: cDeleteStr "delete from xxx where b = 1"
                ?: Right
                    [Instruction opTransaction  0   0 ""
                    ,Instruction opVerifyCookie 234 0 ""]
                /: (getMetadata >>= \mds -> putMetadata [head mds] >> putWriteFlag True)
                -- after do this, label should be 3
                +: cExprWrapperStr "b=1"
                -- delete data from idx_xxx_a
                -- after do this, label should be 6
                +: insertTemp (appendInstructions
                    [Instruction opOpenWrite    1 0 "idx_xxx_a"
                    -- make key (a) for index_xxx_a
                    ,Instruction opColumn       0 0 ""
                    ,Instruction opMakeKey      1 0 ""
                    -- begin index
                    ,Instruction opBeginIdx     1 0 ""
                    ,Instruction opNoop         0 4 ""
                    -- compare address, jump when negative
                    ,Instruction opAddress      0 0 ""
                    ,Instruction opAddress      1 0 ""
                    ,Instruction opJNe          0 5 ""
                    -- delete and break
                    ,Instruction opDelete       1 0 ""
                    ,Instruction opGoto         0 3 ""
                    --
                    ,Instruction opNoop         0 5 ""
                    ,Instruction opNextIdx      0 3 ""
                    ,Instruction opGoto         0 4 ""
                    ,Instruction opNoop         0 3 ""
                    --
                    ,Instruction opClose        1 0 ""])
                -- delte from idx_xxx_a_b
                +: insertTemp (appendInstructions
                    [Instruction opOpenWrite    1 0 "idx_xxx_a_b"
                    -- make key (a,b) for index_xxx_a_b
                    ,Instruction opColumn       0 0 ""
                    ,Instruction opColumn       0 1 ""
                    ,Instruction opMakeKey      2 0 ""
                    -- begin index
                    ,Instruction opBeginIdx     1 0 ""
                    ,Instruction opNoop         0 7 ""
                    -- compare address, jump when negative
                    ,Instruction opAddress      0 0 ""
                    ,Instruction opAddress      1 0 ""
                    ,Instruction opJNe          0 8 ""
                    -- delete and break
                    ,Instruction opDelete       1 0 ""
                    ,Instruction opGoto         0 6 ""
                    --
                    ,Instruction opNoop         0 8 ""
                    ,Instruction opNextIdx      0 6 ""
                    ,Instruction opGoto         0 7 ""
                    ,Instruction opNoop         0 6 ""
                    --
                    ,Instruction opClose        1 0 ""])
                -- delete from table
                +: insertTemp (appendInst opDelete 0 0 "")
                +: removeTemp
                >: Right [Instruction opCommit 0 0 ""]
    ]