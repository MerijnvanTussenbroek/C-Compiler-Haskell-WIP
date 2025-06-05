module CodeGen where

import Algebra
import Library.VMAssemblyLangAST
import AbstractSyntax 

type Prog = Code
type Mems = Code
type Stats = Code
type Exps = Code
type Vars = Code

type Enums = Code
type Structs = Code
type TypDefs = Code

compiler = cFolder codeGen ()

codeGen :: CAlgebra Prog Mems Stats Exps Vars () Enums Structs TypDefs
codeGen = CAlgebra
    dProgram

    dMemberBlock
    dMemberDecl
    dMemberStat
    dMemberFunc
    dMemberEnum
    dMemberStruct
    dMemberInc
    dMemberTypedef

    dEnum

    dStruct

    dDef11
    dDef12
    dDef2
    dDef31
    dDef32

    dStatBlock
    dStatDecl
    dStatExp
    dStatRet
    dStatIfEl
    dStatWhi

    dBinExp
    dUnaExp1
    dUnaExp2
    dFuncCall
    dLitInt
    dLitChar
    dLitDouble
    dLitVar
    dLitPointer
    dLitArray

    dVar
    dArrayVar 
    dEnumVar 
    dStructVar 

dProgram :: () -> Mems -> ((),Prog)
dProgram () = m 

dMemberBlock :: () -> [Mems] -> ((),Mems)
dMemberBlock () = m . concat

dMemberDecl :: () -> Vars -> ((),Mems)
dMemberDecl _ var = ((), var)

dMemberStat :: () -> Stats -> ((), Mems)
dMemberStat _ stat = ((),stat)

dMemberFunc :: () -> VarType -> Identifier -> [Vars] -> Stats -> ((),Mems)
dMemberFunc _ _ id vars stats = ((),[LABEL id] ++ helperFunc (concat vars) ++ stats)

helperFunc :: Vars -> Vars
helperFunc ((DEFINE x):xs) = [DEFINE x, SET x] ++ helperFunc xs
helperFunc [] = []

dMemberEnum :: () -> Enums -> ((),Mems)
dMemberEnum _ _ = ((),[])

dMemberStruct :: () -> Structs -> ((),Mems)
dMemberStruct _ _ = ((),[])

dMemberInc :: () -> String -> ((),Mems)
dMemberInc _ _ = ((),[])

dMemberTypedef :: () -> TypDefs -> ((),Mems)
dMemberTypedef _ _ = ((),[])

dEnum :: () -> Identifier -> Stats -> ((),Enums)
dEnum _ _ _ = ((),[])

dStruct :: () -> Identifier -> [Vars] -> ((),Structs)
dStruct _ _ _ = ((),[])

dDef11 :: () -> Identifier -> [Vars] -> Identifier -> ((),TypDefs)
dDef11 _ _ _ _ = ((),[])

dDef12 :: () -> Identifier -> Identifier -> ((),TypDefs)
dDef12 _ _ _ = ((),[])

dDef2 :: () -> Modifier -> VarType -> Identifier -> ((), TypDefs)
dDef2 _ _ _ _ = ((),[])

dDef31 :: () -> Identifier -> Stats -> Identifier -> ((), TypDefs)
dDef31 _ _ _ _ = ((),[])

dDef32 :: () -> Identifier -> Identifier -> ((),TypDefs)
dDef32 _ _ _ = ((),[])

dStatBlock :: () -> [Stats] -> ((), Stats)
dStatBlock _ stats = ((), concat stats)

dStatDecl :: () -> Vars -> ((),Stats)
dStatDecl _ vars = ((), vars)

dStatExp :: () -> Exps -> ((), Stats)
dStatExp _ exps = ((),exps)

dStatRet :: () -> Exps -> ((), Stats)
dStatRet _ ret = ((),ret ++ [RET])

dStatIfEl :: () -> Exps -> Stats -> Stats -> ((),Stats)
dStatIfEl _ _ _ _ = ((),[])

dStatWhi :: () -> Exps -> Stats -> ((),Stats)
dStatWhi _ _ _ = ((),[])

dBinExp :: () -> Operator -> Exps -> Exps -> ((),Exps)
dBinExp _ Assignment [LOAD x] exps2 = ((),exps2 ++ [SET x])
dBinExp _ op exps1 exps2 = ((), exps1 ++ exps2 ++ [opToOp op])

dUnaExp1 :: () -> Exps -> Operator -> ((),Exps)
dUnaExp1 _ _ _ = ((),[])

dUnaExp2 :: () -> Operator -> Exps -> ((),Exps)
dUnaExp2 _ _ _ = ((),[])

dFuncCall :: () -> Identifier -> [Exps] -> ((), Exps)
dFuncCall _ id exps = ((), concat (reverse exps)++[JUMP1 id])

dLitInt :: () -> Int -> ((),Exps)
dLitInt _ int = ((),[PUSH int])

dLitChar :: () -> Char -> ((),Exps)
dLitChar _ _ = ((),[])

dLitDouble :: () -> Double -> ((),Exps)
dLitDouble _ _ = ((),[])

dLitVar :: () -> Identifier -> ((),Exps)
dLitVar _ id = ((),[LOAD id])

dLitPointer :: () -> VarType -> Identifier -> ((),Exps)
dLitPointer _ _ _ = ((),[])

dLitArray :: () -> Identifier -> Int -> ((),Exps)
dLitArray _ _ _ = ((),[])

dVar :: () -> Modifier -> VarType -> Identifier -> ((), Vars)
dVar _ _ _ id = ((),[DEFINE id])

dArrayVar :: () -> Modifier -> VarType -> Identifier -> Int -> ((), Vars)
dArrayVar _ _ _ _ _ = ((),[])

dEnumVar :: () -> VarType -> Identifier -> ((), Vars)
dEnumVar _ _ _ = ((),[])

dStructVar :: () -> VarType -> Identifier -> ((),Vars)
dStructVar _ _ _ = ((), [])

m :: x -> ((),x)
m x = ((),x)

opToOp :: Operator -> Opcodes
opToOp Add = ADD
opToOp Min = SUB
opToOp Mul = MUL
opToOp Div = DIV