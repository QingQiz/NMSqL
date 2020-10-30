module Test where

import Ast


testCreateTable1 = "create table asd (a int default null, b int primary key desc, c double, d string(5), unique string not null default \"asd\" primary key check(LEN(unique>2)), unique(a,b), primary key(c,d), check(a>1))"
testCreateTable1ShouldBe = Just (
    CreateTable "asd"
        [ColumnDef "a" TInt [ColDefault Null],
         ColumnDef "b" TInt [ColPrimaryKey DESC],
         ColumnDef "c" TDouble [],
         ColumnDef "d" (TString 5) [],
         ColumnDef "unique" (TString 0)
            [ColNotNull,
             ColDefault (ValStr "asd"),
             ColCheck (BinExpr Gr (FunctionCall "LEN" [Column "unique"]) (ConstValue (ValInt 2)))
            ]
        ]
        [TbUnique ["a","b"],
         TbPrimaryKey ["c","d"],
         TbCheck (BinExpr Gr (Column "a") (ConstValue (ValInt 1)))],
    "")
