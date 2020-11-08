module TestCodeGen where


import Ast
import Expr
import TestUtils
import Instruction

import Test.HUnit


cgTestCases :: [CGTestCase]
cgTestCases = [
----------------------------------------------------------
-- Test code generator for expr
----------------------------------------------------------
        -- table-column-expr
          (cExpr (TableColumn "zzz" "a"), Left "No such column: zzz.a")
        , (cExpr (TableColumn "xxx" "d"), Left "No such column: xxx.d")
        , (cExpr (TableColumn "xxx" "b"), Right [Instruction opColumn 0 1 ""])
        , (cExpr (TableColumn "yyy" "b"), Right [Instruction opColumn 1 1 ""])
        -- column-expr
        , (cExpr (Column "e"), Left "No such column: e")
        , (cExpr (Column "a"), Left "Ambiguous column name: a")
        , (cExpr (Column "b"), Left "Ambiguous column name: b")
        , (cExpr (Column "c"), Right [Instruction opColumn 0 2 ""])
        , (cExpr (Column "d"), Right [Instruction opColumn 1 2 ""])
    ]


cgTests :: Test
cgTests =
    let labels = [
            -- table-column-expr
              ("test1", "table-column: wrong table name"    )
            , ("test1", "table-column: wrong column name"   )
            , ("test1", "table-column: right case"          )
            , ("test1", "table-column: right case"          )
            -- column-expr
            , ("test2", "column: wrong column name"         )
            , ("test2", "column: ambigous column name"      )
            , ("test2", "column: ambigous column name"      )
            , ("test2", "column: right case"                )
            , ("test2", "column: right case"                )
            ]
     in connectLabelWithCase labels cgTestCases
