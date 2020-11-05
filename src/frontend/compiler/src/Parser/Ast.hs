module Ast where


----------------------------------------------------------
-- Some Operators
----------------------------------------------------------
data BinOp = Multiple | Divide   -- (*) (/)
           | Plus     | Minus    -- (+) (-)
           | Ls | LE  | Gr | GE  -- (<) (<=) (>) (>=)
           | Eq | NE  | In       -- (= ==) (!= <>) (IN)
           | And| Or             -- (AND) (OR)
           deriving (Show, Eq)


data LikeOp = Like | NotLike     -- (LIKE) (NOT LIKE)
            | Glob | NotGlob     -- (GLOB) (NOT GLOB)
            deriving (Show)


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

data ValueList = ValueList [Value]
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
          deriving (Show)


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

