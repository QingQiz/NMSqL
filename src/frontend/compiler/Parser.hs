module Parser where

import Data.Char
import Control.Monad()
import Control.Applicative


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


-- give a function, return a parser
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \inp ->
    case inp of
         (c:cs) | f c -> Just (c, cs)
                | otherwise -> Nothing
         "" -> Nothing


char = satisfy . (==)
charIgnoreCase c = satisfy $ \inp -> toLower c == toLower inp

stringIgnoreCase "" = return ""
stringIgnoreCase str@(c:cs) = charIgnoreCase c >> stringIgnoreCase cs >> return str

-- (:) <$> (m a)    -> m (a:)
-- m (a:) <*> m [a] -> m [a]
sepBy :: Parser sep -> Parser a -> Parser [a]
sepBy sep a = (:) <$> a <*>  many (sep >> a)
