module Main where

import Library.Library
import Library.ElementaryParsers
import Library.ParserCombinators
import Prelude hiding ((<$>),(<*>),(<|>),(<*))
import Lexer
import Parser
import Algebra
import AnalysisAlgebra 
import Desugering
import AbstractSyntax

main :: IO ()
main = do
    let filepath = "C:/Users/merij/Desktop/hobby's/C_compiler/Example.C"
    input <- readFile filepath
    let x = run lexer input
    print x
    print " "
    let z = run parser x
    print z
    print " "
    let (a,b) = desugerar z
    print a
    print "no problems found"
    
run :: Parser s a -> [s] -> a
run f s = getbestparse x (fst (head x))
    where 
        x = f s
        getbestparse ((a,b):xs) bestparse = if null b then getbestparse xs a else getbestparse xs bestparse
        getbestparse [] bestparse = bestparse
