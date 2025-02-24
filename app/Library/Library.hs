module Library.Library where

import Data.Char
import Prelude hiding ((<$>),(<*>),(<|>),(<<|>),(<*),(*>),(<$))

import Library.ElementaryParsers
import Library.ParserCombinators

-- more complex parsers
parseSpaces :: Parser Char Char
parseSpaces = satisfy isSpace

parseAnySymbol :: Parser Char Char
parseAnySymbol = satisfy isAlphaNum

digit :: Parser Char Int
digit = f <$> satisfy isDigit
    where f c = ord c - ord '0'

capital :: Parser Char Char
capital = satisfy isUpper

lower :: Parser Char Char
lower = satisfy isLower

natural :: Parser Char Int
natural = foldl f 0 <$> greedy digit
    where f a b = 10 * a + b

parseInteger :: Parser Char Int
parseInteger = (\a b -> if a == '-' then -b else b) <$> (satisfy (== '-') <|> succeed ' ') <*> natural

parseDouble :: Parser Char Double
parseDouble = (\a b _ d -> if a == '-' then -(f b d) else f b d ) 
    <$> (satisfy (== '-') <|> succeed ' ') 
    <*> natural 
    <*> satisfy (== '.') 
    <*> natural
    where
        f b d = fromIntegral b + (fromIntegral d / (10 ^ length (show d)))

parseChar :: Parser Char Char
parseChar = satisfy (const True)

option :: Parser s a -> a -> Parser s a
option p r = p <|> succeed r

pack :: Parser s a -> Parser s b -> Parser s c -> Parser s b
pack a b c = (\_ x _ -> x) <$> a <*> b <*> c

choice :: [Parser s a] -> Parser s a
choice = foldr (<|>) failp

greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) failp

greedy :: Parser s b -> Parser s [b]
greedy p = (:) <$> p <*> (greedy p <<|> succeed [])

many :: Parser s b -> Parser s [b]
many p = (:) <$> p <*> (many p <|> succeed [])

listOf :: Parser s a -> Parser s b -> Parser s [a]
listOf p q = ((:) <$> p <*> greedy ((\_ a -> a) <$> q <*> p)) <|> ((: []) <$> p)

parsingList :: Parser s a -> Parser s b -> Parser s c -> Parser s d -> Parser s [d]
parsingList openSymbol closingSymbol seperator listItem = pack openSymbol (listOf listItem seperator) closingSymbol

chainr :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr p q = (output <$> many (middleparser <$> p <*> q) <*> p) <|> p
  where
    middleparser :: a -> (a -> a -> a) -> (a -> a)
    middleparser x op = (x `op`)
    output :: [a -> a] -> a -> a
    output a xs = foldr ($) xs a

chainl :: Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl p q = (output <$> p <*> many (middleparser <$> q <*> p)) <|> p
    where
        output :: a -> [a -> a] -> a
        output a xs = foldl (flip ($)) a xs
        middleparser :: (a -> a -> a) -> a -> (a -> a)
        middleparser op x = (`op` x)

eof :: Show s => Parser s ()
eof xs = if null xs then succeed () xs else failp []
