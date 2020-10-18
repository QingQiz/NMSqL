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
         "" -> Nothing

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

-- match s and return ret
matchAndRet s ret = spcStrIgnoreCase s >> return ret

-- parser surrounded by brackets
surroundByBrackets p = spcChar '(' >> p <* spcChar ')'

-- parser surrounded by quotation mark
surroundByQm p = (many space >> char '"') >> p <* (char '"')

-- parse a string value
strValue = many space >> char '"' >> (concat <$> many tryTakeChar) <* char '"'
    where
        tryTakeChar = check <$> charExcept '"' <*> anyChar
        -- see `https://baike.baidu.com/item/转义字符#字符表`
        check '\\' 'a'  = ['\a']
        check '\\' 'b'  = ['\b']
        check '\\' 'f'  = ['\f']
        check '\\' 'n'  = ['\n']
        check '\\' 'r'  = ['\r']
        check '\\' 't'  = ['\t']
        check '\\' 'v'  = ['\v']
        check '\\' '\\' = ['\\']
        check '\\' '\'' = ['\'']
        check '\\' '"'  = ['"' ]
        check '\\' '?'  = ['?' ]
        check '\\' '0'  = ['\0']
        check c1   c2   = [c1, c2]


-- parse a int value
intValue = toInt <$> (some digit <* (checkChar (/='.')))
    where toInt x = (read x)::Int

-- parse a float value
floatValue = toDouble <$> ((\a b -> a ++ "." ++ b) <$> (some digit) <*> ((char '.' >> some digit) <|> return "0"))
    where toDouble x = (read x)::Double

----------------------------------------------------------
-- SQL parser implementation
----------------------------------------------------------


-- identification matches regex: `[_a-zA-Z]+[_a-zA-Z0-9]*`
ident = (++)
    <$> (many space >> some (letter <|> char '_'))
    <*> (many (letter <|> digit <|> char '_'))


-- sort order ::= asc | desc | default
sortOrder = (spcStrIgnoreCase "asc"  >> return ASC)  <|> -- asc index
            (spcStrIgnoreCase "desc" >> return DESC) <|> -- desc index
            (spcStrIgnoreCase ""     >> return ASC)      -- default is asc index


-- sql         ::= CREATE INDEX index-name
--                 ON table-name ( column-name [,column-name]* )
-- column-name ::= name [ ASC | DESC ]
createIndex = CreateIndex
    <$> (spcStrIgnoreCase "create index " >> ident) -- index name
    <*> (spcStrIgnoreCase "on "           >> ident) -- table name
    <*> surroundByBrackets (sepByOrEmpty (spcChar ',') columnName)
    where columnName = (,)
            <$> ident
            <*> sortOrder


-- sql ::= DROP INDEX index-name
dropIndex = DropIndex <$> (spcStrIgnoreCase "drop index " >> ident)


-- sql               ::= CREATE TABLE table-name (
--                         column-def [,column-def]*
--                         [,table-constraint]*
--                       )
-- column-def        ::= name type [column-constraint]
-- type              ::= INT | DOUBLE | STRING | STRING (string-max-length)
-- column-constraint ::= NOT NULL
--                     | PRIMARY KEY [sort-order]
--                     | UNIQUE
--                     | CHECK (Expr)
--                     | DEFAULT value
-- table-constraint  ::= PRIMARY KEY (name [,name]*)
--                     | UNIQUE (name [,name]*)
--                     | CHECK (Expr)
createTable = CreateTable
    <$> ident
    <*> (spcChar '(' >> sepBy (spcChar ',') columnDef)
    <*> (many (spcChar ',' >> tableConstraint)) <* spcChar ')'
    where
        columnType = (spcStrIgnoreCase "int" >> return TInt)        -- int
                 <|> (spcStrIgnoreCase "double" >> return TDouble)  -- double
                 -- string | string (string length)
                 <|> (TString <$> (spcStrIgnoreCase "string " >> (strLength <|> return 0)))
            where
                strLength = surroundByBrackets intValue

        columnDef = ColumnDef <$> ident <*> columnType <*> many columnConstraint

        tableConstraint = (matchAndRet "primary key" TbPrimaryKey <*> surroundByBrackets (some ident))
                      <|> (matchAndRet "unique" TbUnique <*> surroundByBrackets (some ident))
                      <|> (matchAndRet "check" TbCheck <*> surroundByBrackets expr)

        columnConstraint = (matchAndRet "not null" ColNotNull) -- NOT NULL
                       <|> (ColPrimaryKey <$> primaryKey)      -- PRIARY KEY [ASC | DESC]
                       <|> (matchAndRet "unique" ColUnique)    -- UNIQUE
                       <|> (ColCheck <$> check)                -- CHECK(Expr)
                       <|> (ColDefault <$> defaultVal)         -- DEFAULT value
            where
                primaryKey = spcStrIgnoreCase "primary key " >> sortOrder
                check      = spcStrIgnoreCase "check"        >> surroundByBrackets expr
                defaultVal = spcStrIgnoreCase "default "     >> value


value = (ValStr <$> strValue)
    <|> (ValInt <$> intValue)
    <|> (ValDouble <$> floatValue)

expr = undefined
