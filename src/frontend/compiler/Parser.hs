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

space  = satisfy isSpace
letter = satisfy isLetter
digit  = satisfy isDigit

char = satisfy . (==)
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


----------------------------------------------------------
-- SQL parser implementation
----------------------------------------------------------

-- identification matches regex: `[_a-zA-Z]+[_a-zA-Z0-9]*`
ident = (++)
    <$> (many space >> some (letter <|> char '_'))
    <*> (many (letter <|> digit <|> char '_'))


-- sql         ::= CREATE INDEX index-name
--                 ON table-name ( column-name [,column-name]* )
-- column-name ::= name [ ASC | DESC ]
createIndex = CreateIndex
    <$> (spcStrIgnoreCase "create index " >> ident) -- index name
    <*> (spcStrIgnoreCase "on "           >> ident) -- table name
    <*> (spcChar '(' *> sepByOrEmpty (spcChar ',') columnName <* spcChar ')')
    where columnName = (,)
            <$> ident
            <*> ((spcStrIgnoreCase "asc"  >> return ASC)  <|> -- asc index
                 (spcStrIgnoreCase "desc" >> return DESC) <|> -- desc index
                 (spcStrIgnoreCase ""     >> return ASC))     -- default is asc index


-- sql ::= DROP INDEX index-name
dropIndex = DropIndex <$> (spcStrIgnoreCase "drop index " >> ident)

