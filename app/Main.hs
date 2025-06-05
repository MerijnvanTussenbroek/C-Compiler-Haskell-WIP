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
    putStrLn "Begin lexing\n"
    let x = run lexer input
    print x
    putStrLn "\nBegin Parsing\n"
    let y = run parser x
    let z = reorder y [] [] [] []
    print z
    putStrLn"\nBegin Desugaring\n"
    let (env, ast) = desugerar z
    print ast
    -- we skip the checkers for now, as we do with the desugerar, we will only use correct programs for now
    
run :: Parser s a -> [s] -> a
run parser input = getbestparse x (fst (head x))
    where 
        x = runParse parser input
        getbestparse ((a,b):xs) bestparse = if null b then getbestparse xs a else getbestparse xs bestparse
        getbestparse [] bestparse = bestparse


--We use this function to reorder elements and temporarily discard them
reorder :: Program -> [Members] -> [Members] -> [Members] -> [Members] -> Program
reorder (Program (MemberBlock ((MemberBlock x):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty (MemberBlock x:gl) fu  
reorder (Program (MemberBlock ((MemberDeclaration var):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty (MemberDeclaration var:gl) fu 
reorder (Program (MemberBlock ((MemberStatement stat):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty (MemberStatement stat:gl) fu 
reorder (Program (MemberBlock ((MemberFunction vart id vars stats):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty gl (MemberFunction vart id vars stats:fu)  
reorder (Program (MemberBlock ((MemberEnum enum):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) (MemberEnum enum:en) ty gl fu 
reorder (Program (MemberBlock ((MemberStruct str):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty gl fu --discared
reorder (Program (MemberBlock ((MemberInclude id):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty gl fu --discared
reorder (Program (MemberBlock ((MemberTypedef typdef):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty gl fu --discared
reorder (Program (MemberBlock [])) en ty gl fu = Program (MemberBlock (en++ty++gl++fu)) 
reorder (Program x) _ _ _ _ = Program x
