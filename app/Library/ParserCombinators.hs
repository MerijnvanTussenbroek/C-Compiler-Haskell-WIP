{-# LANGUAGE InstanceSigs #-}
module Library.ParserCombinators where

import Library.ElementaryParsers

import Control.Applicative
import Control.Monad
import Debug.Trace (trace)

-- class definition

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f (Parser s) = Parser $ \input -> [ (f x,xs) | (x,xs) <- s input ]

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure p = Parser $ \input -> [(p, input)]
    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    (Parser p) <*> (Parser q) = Parser $ \input -> [(z x,xs) | (z,zs) <- p input, (x,xs) <- q zs]

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = failp
    (<|>) :: Parser s a -> Parser s a -> Parser s a
    (Parser p) <|> (Parser q) = Parser $ \input -> p input ++ q input

instance Monad (Parser s) where
    return :: a -> Parser s a
    return = pure
    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    p >>= f = Parser $ \input -> [(y,ys) | (x,xs) <- runParse p input, (y,ys) <- runParse (f x) xs]

instance MonadPlus (Parser s) where
    mzero :: Parser s a
    mzero = empty
    mplus :: Parser s a -> Parser s a -> Parser s a
    mplus = (<|>)

infixr 3 <<|>
(<<|>) :: Parser s a -> Parser s a -> Parser s a
p <<|> q = trace   "<<|>" Parser $ \input -> let r = runParse p input in if null r then runParse q input else r