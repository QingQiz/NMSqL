{-# LANGUAGE LambdaCase #-}
module Ast where

import Data.List


----------------------------------------------------------
-- SQL
----------------------------------------------------------
data SQL = SQLIndex IndexAction
         | SQLTable TableActon
         | SQLDelete Delete
         | SQLInsert Insert
         | SQLUpdate Update
         | SQLSelect Select

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
            deriving Eq


data CompoundOp = Union     | UnionAll
                | Intersect | Except


data Type = TInt | TString Int | TDouble


data SortOrder = ASC | DESC


----------------------------------------------------------
-- Some help data type
----------------------------------------------------------
data Value = ValStr String
           | ValInt Int
           | ValDouble Double
           | Null


data ValueList = ValueList [Expr]
               | SelectResult Select


data ColumnConstraint = ColNotNull
                      | ColPrimaryKey SortOrder
                      | ColUnique
                      | ColCheck Expr
                      | ColDefault Value


data TableContraint = TbPrimaryKey [ColumnName]
                    | TbUnique [ColumnName]
                    | TbCheck Expr


data ColumnDef = ColumnDef ColumnName Type [ColumnConstraint]


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
          | EmptyExpr                        -- place holder


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
        IsNull       e        -> brackets $ show e  ++ " IS NUll"
        Between      e1 e2 e3 -> brackets $ show e1 ++ " BETWEEN " ++ show e2 ++ " AND " ++ show e3
        InExpr       e vl     -> brackets $ show e  ++ " IN "      ++ show vl
        NotExpr      e        -> brackets $ "NOT "  ++ show e
        SelectExpr   sel      -> brackets $ show sel
        Column       cn       -> cn
        TableColumn  tn cn    -> tn ++ "." ++ cn
        AnyColumn             -> "*"
        EmptyExpr             -> "EmptyExpr"
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

instance Show LikeOp where
    show Like    = "LIKE"
    show NotLike = "NOT LIKE"
    show Glob    = "GLOB"
    show NotGlob = "NOT GLOB"

instance Show CompoundOp where
    show Union     = "UNION"
    show UnionAll  = "UNION ALL"
    show Intersect = "INTERSECT"
    show Except    = "EXCEPT"

instance Show Value where
    show Null       = "NULL"
    show (ValInt i) = show i
    show (ValStr s) = show s
    show (ValDouble d) = show d

instance Show ValueList where
    show (ValueList vl)     = "(" ++ intercalate "," (map show vl) ++ ")"
    show (SelectResult sel) = "(" ++ show sel ++ ")"

instance Show Type where
    show TInt = "int"
    show (TString len) = "string(" ++ show len ++ ")"
    show TDouble = "double"

instance Show SortOrder where
    show ASC = "ASC"
    show DESC = "DESC"

instance Show ColumnConstraint where
    show ColNotNull = "NOT NULL"
    show (ColPrimaryKey ord) = "PRIMARY KEY " ++ show ord
    show ColUnique = "UNIQUE"
    show (ColCheck expr) = "CHECK(" ++ show expr ++ ")"
    show (ColDefault val) = "DEFAULT " ++ show val

instance Show TableContraint where
    show (TbPrimaryKey cs) = "PRIMARY KEY(" ++ intercalate "," cs ++ ")"
    show (TbUnique cs) = "UNIQUE(" ++ intercalate "," cs ++ ")"
    show (TbCheck expr) = "CHECK(" ++ show expr ++ ")"

instance Show ColumnDef where
    show (ColumnDef name tp colCtt) = name ++ " " ++ show tp ++ concatMap (\x -> " " ++ show x) colCtt

instance Show TableActon where
    show (CreateTable name colDef tbCtt) =
        let colDef' = intercalate "," (map show colDef)
            tbCtt'  = concatMap (\x -> "," ++ show x) tbCtt
         in "CREATE TABLE " ++ name ++ " (" ++ colDef' ++ tbCtt' ++ ")"
    show (DropTable name) = "DROP TABLE " ++ name
