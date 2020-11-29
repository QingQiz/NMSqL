{-# LANGUAGE LambdaCase #-}
module Ast where

import Data.List


----------------------------------------------------------
-- Some Operators
----------------------------------------------------------
data BinOp = Multiply | Divide   -- (*) (/)
           | Plus     | Minus    -- (+) (-)
           | Ls | LE  | Gt | GE  -- (<) (<=) (>) (>=)
           | Eq | NE             -- (= ==) (!= <>)
           | And| Or             -- (AND) (OR)
           deriving Eq


data LikeOp = Like | NotLike     -- (LIKE) (NOT LIKE)
            | Glob | NotGlob     -- (GLOB) (NOT GLOB)
            deriving (Show, Eq)


data CompoundOp = Union     | UnionAll
                | Intersect | Except
                deriving (Show)


data Type = TInt | TString Int | TDouble deriving (Show)


data SortOrder = ASC | DESC deriving (Show)


----------------------------------------------------------
-- Some help data type
----------------------------------------------------------
data Value = ValStr String
           | ValInt Int
           | ValDouble Double
           | Null
           deriving (Show)

data ValueList = ValueList [Expr]
               | SelectResult Select
               deriving (Show)


data ColumnConstraint = ColNotNull
                      | ColPrimaryKey SortOrder
                      | ColUnique
                      | ColCheck Expr
                      | ColDefault Value
                      deriving (Show)


data TableContraint = TbPrimaryKey [ColumnName]
                    | TbUnique [ColumnName]
                    | TbCheck Expr
                    deriving (Show)


data ColumnDef = ColumnDef ColumnName Type [ColumnConstraint] deriving (Show)


data Expr = BinExpr BinOp Expr Expr          -- 2 binOp 3
          | LikeExpr LikeOp Expr Expr        -- 2 like 3
          | ConstValue Value                 -- value
          | FunctionCall FuncName [Expr]     -- funcname(expr...)
          | IsNull Expr                      -- 1 is null
          | Between Expr Expr Expr           -- 1 between 2 and 3
          | InExpr Expr ValueList            -- 1 in 2
          | NotExpr Expr                     -- not 1
          | SelectExpr Select                -- (1)
          | Column ColumnName                -- column
          | AnyColumn                        -- column *
          | TableColumn TableName ColumnName -- table.column


----------------------------------------------------------
-- Some Type Alias
----------------------------------------------------------
type FuncName = String
type TableName = String
type ColumnName = String
type IndexName = String

----------------------------------------------------------
-- Select Stmt
----------------------------------------------------------
data Select = Select {
    selectResult    :: [(Expr, String)]         , -- expression AS result-string
    selectTableName :: [TableName]              , -- table-name
    selectWhere     :: Maybe Expr               , -- WHERE expression
    selectGroupBy   :: [Expr]                   , -- GROUP BY expression-list
    selectHaving    :: Maybe Expr               , -- HAVING expression
    selectUnion     :: [(CompoundOp, Select)]   , -- compound-op select
    selectSortOrder :: [(Expr, SortOrder)]        -- ORDER BY expr [sort-order]
} deriving (Show)


----------------------------------------------------------
-- Update Stmt
----------------------------------------------------------
data Update = Update TableName [(ColumnName, Expr)] (Maybe Expr)
            deriving (Show)


----------------------------------------------------------
-- Insert Stmt
----------------------------------------------------------
data Insert = Insert TableName [ColumnName] ValueList
            deriving (Show)

----------------------------------------------------------
-- Delete Stmt
----------------------------------------------------------
data Delete = Delete TableName (Maybe Expr)
            deriving (Show)

----------------------------------------------------------
-- Create and Drop Table Stmt
----------------------------------------------------------
data TableActon = CreateTable TableName [ColumnDef] [TableContraint]
                | DropTable TableName
                deriving (Show)

----------------------------------------------------------
-- Create and Drop Index Stmt
----------------------------------------------------------
data IndexAction = CreateIndex IndexName TableName [(ColumnName, SortOrder)]
                 | DropIndex IndexName
                 deriving (Show)


----------------------------------------------------------
-- Instance Show for Expr
----------------------------------------------------------
instance Show Expr where
    show = \case
        BinExpr      op e1 e2 -> brackets $ show e1 ++ show op ++ show e2
        LikeExpr     op e1 e2 -> brackets $ show e1 ++ " "     ++ show op ++ " " ++ show e2
        ConstValue   val      -> show val
        FunctionCall fn es    -> fn                 ++ brackets (intercalate "," $ map show es)
        IsNull       e        -> brackets $ show e  ++ " IsNull"
        Between      e1 e2 e3 -> brackets $ show e1 ++ " Between " ++ show e2 ++ " And " ++ show e3
        InExpr       e vl     -> brackets $ show e  ++ " In "      ++ show vl
        NotExpr      e        -> brackets $ "Not "  ++ show e
        SelectExpr   sel      -> brackets $ show sel
        Column       cn       -> cn
        TableColumn  tn cn    -> tn ++ "." ++ cn
        AnyColumn             -> "*"
        where brackets s = "(" ++ s ++ ")"

instance Show BinOp where
    show Multiply = "*"
    show Divide   = "/"
    show Plus     = "+"
    show Minus    = "-"
    show Gt       = ">"
    show GE       = ">="
    show Ls       = "<"
    show LE       = "<="
    show Eq       = "=="
    show NE       = "!="
    show And      = " And "
    show Or       = " Or "