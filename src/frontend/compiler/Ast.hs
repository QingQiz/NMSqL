module Ast where


----------------------------------------------------------
-- Some Operators
----------------------------------------------------------
data BinOp = Multiple | Divide   -- (*) (/)
           | Plus     | Minus    -- (+) (-)
           | Ls | LE  | Gr | GE  -- (<) (<=) (>) (>=)
           | Eq | NEq | In       -- (= ==) (!= <>) (IN)
           | And| Or             -- (AND) (OR)


data LikeOp = Like | NotLike     -- (LIKE) (NOT LIKE)
            | Glob | NotGlob     -- (GLOB) (NOT GLOB)


data UnionOp = Union | UnionAll
             | Intersect | Except


data Type = TInt | TString | TDouble


data SortOrder = ASC | DESC


----------------------------------------------------------
-- Some help data type
----------------------------------------------------------
data ColumnConstraint = ColNotNull
                      | ColPrimaryKey SortOrder
                      | ColUnique
                      | ColCheck Expr
                      | ColDefault String


data TableContraint = TbPrimaryKey [ColumnName]
                    | TbUnique [ColumnName]
                    | TbCheck Expr


data ColumnDef = ColumnDef ColumnName Type [ColumnConstraint]
               

data Expr = BinExpr BinOp Expr Expr          -- 2 binOp 3
          | LikeExpr LikeOp Expr Expr        -- 2 like 3
          | TableColumn TableName ColumnName -- table.column
          | Const String                     -- value
          | FunctionCall FuncName [Expr]     -- funcname(expr...)
          | IsNull Expr                      -- 1 is null
          | Between Expr Expr Expr           -- 1 between 2 and 3
          | InExpr Expr Expr                 -- 1 in 2
          | InValueList Expr [Value]         -- expr in valuelist
          | InSelect Expr Select             -- expr in select
          | NotExpr Expr                     -- not 1
          | SelectExpr Select


----------------------------------------------------------
-- Some Type Alias
----------------------------------------------------------
type FuncName = String
type TableName = String
type ColumnName = String
type IndexName = String
type Value = String

----------------------------------------------------------
-- Select Stmt
----------------------------------------------------------
data Select = Select Select [TableName]
            | SelectWhere Select Expr
            | SelectGroup Select [Expr]
            | SelectHaving Select Expr
            | SelectUnion Select [(UnionOp, Select)]
            | SelectOrderBy Select [(SortOrder, Expr)]


----------------------------------------------------------
-- Update Stmt
----------------------------------------------------------
data Update = Update TableName [(ColumnName, Expr)]
            | UpdateWhere Update Expr


----------------------------------------------------------
-- Insert Stmt
----------------------------------------------------------
data Insert = Insert TableName [ColumnName] [Value]
            | InsertFromSelect TableName [ColumnName] Select

----------------------------------------------------------
-- Delete Stmt
----------------------------------------------------------
data Delete = Delete TableName
            | DeleteWhere TableName Expr

----------------------------------------------------------
-- Create Table Stmt
----------------------------------------------------------
data CreateTable = CreateTable TableName [ColumnDef] [TableContraint]

----------------------------------------------------------
-- Drop Table Stmt
----------------------------------------------------------
data DropTable = DropTable TableName

----------------------------------------------------------
-- Create Index Stmt
----------------------------------------------------------
data CreateIndex = CreateIndex IndexName TableName [(ColumnName, SortOrder)]

----------------------------------------------------------
-- Drop Index Stmt
----------------------------------------------------------
data DropIndex = DropIndex IndexName
