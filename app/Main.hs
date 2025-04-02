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
    let filepath =  "C:/Users/merij/Desktop/hobby's/C_compiler/Example.C"
    --putStrLn "Input path"
    --filepath <- getLine
    input <- readFile filepath
    --putStrLn "1: compile to assembly 2: Visualize 3: evaluate"
    --mode <- getLine
    print "Begin lexing"
    let x = run lexer input
    print "Finished lexing, beginning parsing"
    let z = run parse x
    print "Finishes parsing, begin desugaring"
    let (a,b) = desugerar z
    print a
    print "desugaring done, no problems found"
    
run :: Parser s a -> [s] -> a
run parser input = getbestparse x (fst (head x))
    where 
        x = runParse parser input
        getbestparse ((a,b):xs) bestparse = if null b then getbestparse xs a else getbestparse xs bestparse
        getbestparse [] bestparse = bestparse
