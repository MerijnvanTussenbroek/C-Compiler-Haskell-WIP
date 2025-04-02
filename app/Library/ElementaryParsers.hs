module Library.ElementaryParsers where

import Data.Char

newtype Parser input output = Parser { runParse :: [input] -> [(output,[input])] }

anySymbol :: Parser s s
anySymbol = Parser $ \input -> case input of
        (x:xs) -> [(x,xs)]
        [] -> []

satisfy :: (s -> Bool) -> Parser s s
satisfy f = Parser $ \input -> case input of
        (x:xs)  | f x -> [(x,xs)]
                | otherwise -> []
        [] -> []

symbol :: Eq s => s -> Parser s s
symbol s = satisfy (== s)

token :: Eq s => [s] -> Parser s [s]
token token = Parser $ \input -> case input of
        xs      | token == take l xs -> [(token, drop l xs)]
                | otherwise -> []
        where
                l = length token

nottoken :: Eq s => [s] -> Parser s [s]
nottoken token = Parser $ \input -> case input of
        xs      | token /= take l xs -> [(token, drop 1 xs)]
                | otherwise -> []
        where
                l = length token

failp :: Parser s a
failp = Parser $ \input -> []

succeed :: a -> Parser s a
succeed forcedInput = Parser $ \input -> [(forcedInput, input)]

eof :: Show s => Parser s ()
eof = Parser $ \input -> if null input then [((),input)] else []
