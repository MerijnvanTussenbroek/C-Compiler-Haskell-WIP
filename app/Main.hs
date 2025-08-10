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
import Library.VMAssemblyLangAST 
import CodeGen

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
    let (env2, ast2) = analyse ast
    print ast2
    putStrLn "\nBegin compiling\n"
    let (a,b) = compiler ast
    let c = addHALT False b
    let d = prettyPrinter c
    putStrLn d
    -- we skip the checkers for now, as we do with the desugerar, we will only use correct programs for now
    
run :: Parser s a -> [s] -> a
run parser input = getbestparse x (fst (head x))
    where 
        x = runParse parser input
        getbestparse ((a,b):xs) bestparse = if null b then getbestparse xs a else getbestparse xs bestparse
        getbestparse [] bestparse = bestparse


--We use this function to reorder elements and temporarily discard them

-- I need to fix it as this reverses things
reorder :: Program -> [Members] -> [Members] -> [Members] -> [Members] -> Program
reorder (Program (MemberBlock ((MemberBlock x):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty (gl++[MemberBlock x]) fu  
reorder (Program (MemberBlock ((MemberDeclaration var):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty (gl++[MemberDeclaration var]) fu 
reorder (Program (MemberBlock ((MemberStatement stat):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty (gl++[MemberStatement stat]) fu 
reorder (Program (MemberBlock ((MemberFunction vart id vars stats):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty gl (fu++[MemberFunction vart id vars stats])  
reorder (Program (MemberBlock ((MemberEnum enum):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) (en++[MemberEnum enum]) ty gl fu 
reorder (Program (MemberBlock ((MemberStruct str):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty gl fu --discared
reorder (Program (MemberBlock ((MemberInclude id):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en ty gl fu --discared
reorder (Program (MemberBlock ((MemberTypedef typdef):xs))) en ty gl fu = reorder (Program (MemberBlock xs)) en (ty ++ [MemberTypedef typdef]) gl fu
reorder (Program (MemberBlock [])) en ty gl fu = Program (MemberBlock (en++ty++gl++fu)) 
reorder (Program x) _ _ _ _ = Program x

addHALT :: Bool -> Prog -> Prog
addHALT _ ((LABEL "main"):xs) = LABEL "main" : addHALT True xs
addHALT True ((PUSH 1):RET:xs) = HALT : addHALT False xs
addHALT False (x:xs) = x : addHALT False xs
addHALT y (x:xs) = x : addHALT y xs
addHALT _ [] = []