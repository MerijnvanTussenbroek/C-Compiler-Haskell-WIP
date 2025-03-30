module Main where

import Library.Library
import Library.ElementaryParsers
import Library.ParserCombinators
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
    print "Begin lexing"
    let x = run lexer input
    print x
    print "Finished lexing, beginning parsing"
    let z = run parse x
    print z
    print " "
    let (a,b) = desugerar z
    print a
    print "no problems found"
    
run :: Parser s a -> [s] -> a
run parser input = getbestparse x (fst (head x))
    where 
        x = runParse parser input
        getbestparse ((a,b):xs) bestparse = if null b then getbestparse xs a else getbestparse xs bestparse
        getbestparse [] bestparse = bestparse
