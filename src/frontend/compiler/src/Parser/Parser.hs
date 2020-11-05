module Parser where

import Ast

import Data.Char
import Control.Monad()
import Control.Applicative


----------------------------------------------------------
-- create Parser type supports:
-- Functor, Applicative, Monad and Alternative
----------------------------------------------------------

-- Parser is a monad
newtype Parser a = Parser {
    parse :: String -> Maybe (a, String)
}


instance Functor Parser where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f pa = Parser $ \inp ->
        case parse pa inp of
             Nothing -> Nothing
             Just (a, rst) -> Just (f a, rst)


instance Applicative Parser where
    -- pure :: a -> f a
    pure x = Parser $ \inp -> Just (x, inp)
    -- (<*>) :: f (a -> b) -> f a -> f b
    pfab <*> pa = Parser $ \inp ->
        case parse pfab inp of
             Nothing -> Nothing
             Just (fab, rst) -> parse (fab <$> pa) rst


instance Monad Parser where
    -- (>>=) :: m a -> (a -> m b) -> m b
    pa >>= apb = Parser $ \inp ->
        case parse pa inp of
             Nothing -> Nothing
             Just (a, rst) -> parse (apb a) rst


instance Alternative Parser where
    -- empty :: f a
    empty = Parser $ const Nothing
    -- (<|>) :: f a -> f a -> f a
    pa <|> pb = Parser $ \inp ->
        case parse  pa inp of
             Nothing -> parse pb inp
             x -> x


----------------------------------------------------------
-- some help functions
----------------------------------------------------------

-- give a function, return a parser
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \inp ->
    case inp of
         (c:cs) | f c -> Just (c, cs)
                | otherwise -> Nothing
         "" -> Nothing

checkChar :: (Char -> Bool) -> Parser Char
checkChar f = Parser $ \inp ->
    case inp of
         (c:_) | f c -> Just (c, inp)
               | otherwise -> Nothing
         "" | f '\0' -> Just ('\0', inp)
            | otherwise -> Nothing


-- peek the next char to parse
peek = checkChar (const True)

space  = satisfy isSpace
letter = satisfy isLetter
digit  = satisfy isDigit

-- parse success for any char
anyChar = satisfy (const True)

-- parse fail for any char
alwaysFail = satisfy (const False)

char = satisfy . (==)
charExcept = satisfy . (/=)

charIgnoreCase c = satisfy $ \inp -> toLower c == toLower inp

-- parse a char surrounded by space
spcChar c = many space *> char c <* many space

string "" = return ""
string str@(c:cs) = char c >> string cs >> return str

stringIgnoreCase "" = return ""
stringIgnoreCase str@(c:cs) = charIgnoreCase c >> stringIgnoreCase cs >> return str

-- parse a string surrounded by space
spcStrIgnoreCase str = many space *> stringIgnoreCase str <* many space

-- (:) <$> (m a)    -> m (a:)
-- m (a:) <*> m [a] -> m [a]
sepBy :: Parser sep -> Parser a -> Parser [a]
sepBy sep a = (:) <$> a <*> many (sep >> a)

sepByOrEmpty sep a = sepBy sep a <|> return []

-- parse string like: a,a,a,a
argsList x = sepBy (spcChar ',') x
argsListOrEmpty x = sepByOrEmpty (spcChar ',') x

-- parse string like ((a op b) op c)
-- result:    op
--           /  \
--         op    c
--        /  \
--       a    b
chainl :: Parser (a -> a -> a) -> Parser a -> Parser a
chainl op a = many space >> a >>= forRest
    where
        forRest left = (many space >> op <*> return left <*> a >>= forRest) <|> return left

-- parse string like: op (op a)
-- result: op
--        /
--      op
--     /
--    a
unaryChain :: Parser (a -> a) -> Parser a -> Parser a
unaryChain op a = many (many space >> op) >>= foldr (<$>) a

-- match s and return ret
matchAndRet s ret = spcStrIgnoreCase s >> return ret
matchTwoAndRet s1 s2 ret = spcStrIgnoreCase s1 >> spcStrIgnoreCase s2 >> return ret

-- parser surrounded by brackets
surroundByBrackets p = spcChar '(' >> p <* spcChar ')'
maySurroundByBrackets p = surroundByBrackets p <|> p

-- parser surrounded by quotation mark
surroundByQm p = (many space >> char '"') >> p <* (char '"')

-- parse a string value
strValue = many space >> char '"' >> (concat <$> many tryTakeChar) <* char '"'
    where
        tryTakeChar = (transform <$> (char '\\' >> anyChar)) <|> (charExcept '"' >>= \x -> return [x])
        -- see `https://baike.baidu.com/item/转义字符#字符表`
        transform 'a'  = ['\a']
        transform 'b'  = ['\b']
        transform 'f'  = ['\f']
        transform 'n'  = ['\n']
        transform 'r'  = ['\r']
        transform 't'  = ['\t']
        transform 'v'  = ['\v']
        transform '\\' = ['\\']
        transform '\'' = ['\'']
        transform '"'  = ['"' ]
        transform '?'  = ['?' ]
        transform '0'  = ['\0']
        transform c    = ['\\', c]

-- parse a int value
intValue = toInt <$> (some digit <* (checkChar (/='.')))
    where toInt x = (read x)::Int

-- parse a float value
floatValue = toDouble <$> ((\a b -> a ++ "." ++ b) <$> (some digit) <*> ((char '.' >> some digit) <|> return "0"))
    where toDouble x = (read x)::Double

----------------------------------------------------------
-- SQL parser implementation
----------------------------------------------------------

-- sql ::= "string" | int  | float
value = (ValStr <$> strValue)       <|>
        (ValInt <$> intValue)       <|>
        (ValDouble <$> floatValue)  <|>
        (matchAndRet "null" Null)


-- identification matches regex: `[_a-zA-Z]+[_a-zA-Z0-9]*`
ident = (++)
    <$> (many space >> some (letter <|> char '_'))
    <*> (many (letter <|> digit <|> char '_'))


-- sort order ::= asc | desc | default
sortOrder = (spcStrIgnoreCase "asc"  >> return ASC)  <|> -- asc index
            (spcStrIgnoreCase "desc" >> return DESC) <|> -- desc index
            (spcStrIgnoreCase ""     >> return ASC)      -- default is asc index


-- sql         ::= CREATE INDEX index-name
--                 ON table-name \( column-name [,column-name]* \)
-- column-name ::= name [ ASC | DESC ]
createIndex = CreateIndex
    <$> (spcStrIgnoreCase "create index " >> ident) -- index name
    <*> (spcStrIgnoreCase "on "           >> ident) -- table name
    <*> surroundByBrackets (argsListOrEmpty columnName)
    where columnName = (,)
            <$> ident
            <*> sortOrder


-- sql ::= DROP INDEX index-name
dropIndex = DropIndex <$> (spcStrIgnoreCase "drop index " >> ident)


-- sql ::= CREATE TABLE table-name \( column-def [,column-def]* [,table-constraint]* \)
createTable = (matchTwoAndRet "create" "table" CreateTable)
    <*> ident
    <*> (spcChar '(' >> argsList columnDef)
    <*> (many (spcChar ',' >> tableConstraint)) <* spcChar ')'
    where
        -- column-type ::= int | double | string [\( int-value \)]
        columnType = (spcStrIgnoreCase "int" >> return TInt)        -- int
                 <|> (spcStrIgnoreCase "double" >> return TDouble)  -- double
                 -- string | string (string length)
                 <|> (TString <$> (spcStrIgnoreCase "string" >> (strLength <|> return 0)))
            where
                strLength = surroundByBrackets intValue

        -- column-def        ::= name type [column-constraint]
        columnDef = ColumnDef <$> ident <*> columnType <*> many columnConstraint

        -- table-constraint ::= primary-key \( column-name [,column-name]* \)
        --                    | unique \( column-name [,column-name]* \)
        --                    | check \( expr \)
        tableConstraint = (matchTwoAndRet "primary" "key" TbPrimaryKey <*> surroundByBrackets (argsList ident))
                      <|> (matchAndRet "unique" TbUnique <*> surroundByBrackets (argsList ident))
                      <|> (matchAndRet "check" TbCheck <*> surroundByBrackets expr)

        -- colimn-constraint ::= not null
        --                     | primary key [asc | desc]
        --                     | unique
        --                     | check \( expr \)
        --                     | default value
        columnConstraint = (matchTwoAndRet "not" "null" ColNotNull) -- NOT NULL
                       <|> (matchTwoAndRet "primary" "key" ColPrimaryKey <*> sortOrder) -- PRIARY KEY [ASC | DESC]
                       <|> (matchAndRet "unique" ColUnique)    -- UNIQUE
                       <|> (ColCheck <$> check)                -- CHECK(Expr)
                       <|> (ColDefault <$> defaultVal)         -- DEFAULT value
            where
                check      = spcStrIgnoreCase "check"        >> surroundByBrackets expr
                defaultVal = spcStrIgnoreCase "default "     >> value


-- sql ::= DROP TABLE table-name
dropTable = matchTwoAndRet "drop" "table" DropTable <*> ident


-- operator precedence:
--
-- PD1: (OR AND)                               <
-- PD2: (NOT)                                  <
-- PD3: (ISNULL NOTNULL)                       <
-- PD4: (< > = <= >= <> != ==)                 <
-- PD5: (BETWEEN IN LIKE NOTLIKE GLOB NOTGLOB) <
-- PD6: (+ -)                                  <
-- PD7: (* /)
--
-- expr ::= pd1
expr :: Parser Expr
expr = pd1 where
    pNode opStr op ret = matchAndRet opStr op >>= (\x -> return (ret x))
    pBinNode optStr op = pNode optStr op BinExpr

    -- pd1 ::= pd2 [(and | or) pd2]*
    pd1 = chainl (pBinNode "and " And <|> pBinNode "or " Or) pd2

    -- pd2 ::= [not]* pd3
    pd2 = unaryChain (matchAndRet "not " NotExpr) pd3

    -- pd3 ::= pd4 [is null | not null]
    pd3 = pd4 >>= (\x -> (matchTwoAndRet "is"  "null" (IsNull x)           <|>
                          matchTwoAndRet "not" "null" (NotExpr $ IsNull x) <|>
                          return x))

    -- pd4 ::= pd5 [(< | <= | <> | < | >= | > | == | = | !=) pd5]*
    pd4 = chainl opList pd5
        where
            opToData = [("<=", LE), ("<>", NE), ("<", Ls), (">=", GE), (">", Gr), ("==", Eq), ("=", Eq), ("!=", NE)]
            opList'  = map (\(a, b) -> pBinNode a b) opToData
            opList   = foldl (\z x -> z <|> x) (head opList') (tail opList')

    -- pd5 ::= pd6 between pd6 and pd6
    --       | pd6 in value-list
    --       | pd6 (like | not like | glob | not glob) pd6
    --       | pd6
    -- value-list ::= \( value [, value]* \)
    pd5 = pd6 >>= (\x -> betweenExpr x <|> inExpr x <|> likeExpr x <|> return x)
        where
            betweenExpr = \x -> makeNode x <*> pd6 <*> (spcStrIgnoreCase "and" >> pd6)
                where makeNode x = matchAndRet "between" (Between x) <|>
                                   matchTwoAndRet "not" "between" (\a b -> NotExpr $ Between x a b)

            inExpr = \x -> makeNode x <*> (SelectResult <$> maySurroundByBrackets select <|> ValueList <$> surroundByBrackets (argsListOrEmpty value))
                where makeNode x = matchAndRet "in" (InExpr x) <|> matchTwoAndRet "not" "in" (NotExpr . InExpr x)

            likeExpr = \x -> makeNode x <*> pd6
                where makeNode x = matchAndRet "like" (LikeExpr Like x) <|>
                                   matchAndRet "glob" (LikeExpr Glob x) <|>
                                   matchTwoAndRet "not" "like" (NotExpr . LikeExpr NotLike x) <|>
                                   matchTwoAndRet "not" "glob" (NotExpr . LikeExpr NotGlob x)

    -- pd6 ::= pd7 [(+ | -) pd7]*
    pd6 = chainl (pBinNode "+" Plus     <|> pBinNode "-" Minus ) pd7

    -- pd7 ::= pd0 [(* | /) pd0]*
    pd7 = chainl (pBinNode "*" Multiple <|> pBinNode "/" Divide) pd0

    -- pd0 ::= \( pd1 \)
    --       | table-name . column-name
    --       | column-name
    --       | value
    pd0 = surroundByBrackets pd1                                                                <|>
          (TableColumn  <$> ident <*> (spcChar '.' >> ident))                                   <|>
          (FunctionCall <$> map toLower <$> ident <*> surroundByBrackets (argsListOrEmpty pd1)) <|>
          (Column       <$> ident)                                                              <|>
          (ConstValue   <$> value)                                                              <|>
          (SelectExpr   <$> surroundByBrackets select)


-- sql ::= INSERT INTO table-name [\( column-list \)] VALUES \( value-list \)
--       | INSERT INTO table-name [\( column-list \)] select-stmt
insert = matchTwoAndRet "insert" "into" Insert
    <*> ident
    <*> (surroundByBrackets (argsList ident) <|> return [])
    <*> (matchAndRet "values" ValueList <*> (surroundByBrackets (argsList value)) <|> SelectResult <$> maySurroundByBrackets select)


-- sql ::= UPDATE table-name SET assignment [, assignment]* [WHERE expression]
-- assignment ::= column-name = expression
update = matchAndRet "update" Update
    <*> ident
    <*> (spcStrIgnoreCase "set" >> argsList assignment)
    <*> (matchAndRet "where" Just <*> expr <|> return Nothing)
    where assignment = (,) <$> ident <*> (spcChar '=' >> expr)


-- sql ::= DELETE FROM table-name [WHERE expression]
delete = matchTwoAndRet "delete" "from" Delete
    <*> ident
    <*> (matchAndRet "where" Just <*> expr <|> return Nothing)


-- sql  ::= SELECT result FROM table-list
--          [WHERE expression]
--          [GROUP BY expr-list]
--          [HAVING expression]
--          [compound-op select]*
--          [ORDER BY sort-expr-list]
-- xxx-list ::= xxx[,xxx]*
-- result   ::= * | column-result-list
select = matchAndRet "select" Select
    <*> (argsList resultCol <|> (spcChar '*' >> return [(AnyColumn, "")]))
    <*> (spcStrIgnoreCase "from" >> argsList ident)
    <*> (matchAndRet "where" Just <*> expr <|> return Nothing)
    <*> ((matchTwoAndRet "group" "by" id >> argsList expr) <|> return [])
    <*> (matchAndRet "having" Just <*> expr <|> return Nothing)
    <*> (many ((,) <$> compoundOp <*> maySurroundByBrackets (surroundByBrackets select)))
    <*> ((matchTwoAndRet "order" "by" id >> argsList sortExpr) <|> return [])
    where
        -- column-result ::= expression [AS string]
        resultCol = (,) <$> expr <*> ((spcStrIgnoreCase "as" >> ident) <|> return "")

        -- compound-op   ::= UNION | UNION ALL | INTERSECT | EXCEPT
        compoundOp = matchTwoAndRet "union" "all" UnionAll  <|>
                     matchAndRet    "union"       Union     <|>
                     matchAndRet    "intersect"   Intersect <|>
                     matchAndRet    "except"      Except

        -- sort-expr     ::= expr [sort-order]
        sortExpr = (,) <$> expr <*> sortOrder
