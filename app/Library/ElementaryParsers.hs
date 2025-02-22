module Library.ElementaryParsers where

import Data.Char

-- parser type
type Parser input output = [input] -> [(output,[input])]

-- elementary parsers
anySymbol :: Parser s s
anySymbol (x:xs) = [(x,xs)]
anySymbol [] = []

symbol :: Eq s => s -> Parser s s
symbol s = satisfy (== s)

satisfy :: (s -> Bool) -> Parser s s
satisfy _ []    = []
satisfy f (x:xs)| f x = [(x,xs)]
                | otherwise  = []

token :: Eq s => [s] -> Parser s [s]
token _ []  = []
token a b   | a == take l b = [(a, drop l b)]
            | otherwise = []
    where
        l = length a

failp :: Parser s a
failp _ = []

succeed :: a -> Parser s a
succeed s xs = [(s,xs)]

epsilon :: Parser s ()
epsilon = succeed ()
