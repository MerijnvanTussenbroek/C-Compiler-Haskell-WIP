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
    where
        helperFunc ((DEFINE x):xs) = [DEFINE x, SET x] ++ helperFunc xs
        helperFunc [] = []

dMemberEnum :: () -> Enums -> ((),Mems)
dMemberEnum _ _ = ((),[]) --yet to be implemented

dMemberStruct :: () -> Structs -> ((),Mems)
dMemberStruct _ _ = ((),[]) --yet to be implemented

dMemberInc :: () -> String -> ((),Mems)
dMemberInc _ _ = ((),[]) --yet to be implemented

dMemberTypedef :: () -> TypDefs -> ((),Mems)
dMemberTypedef _ _ = ((),[]) --yet to be implemented

dEnum :: () -> Identifier -> Stats -> ((),Enums)
dEnum _ _ _ = ((),[]) --yet to be implemented

dStruct :: () -> Identifier -> [Vars] -> ((),Structs)
dStruct _ _ _ = ((),[]) --yet to be implemented

dDef11 :: () -> Identifier -> [Vars] -> Identifier -> ((),TypDefs)
dDef11 _ _ _ _ = ((),[]) --yet to be implemented

dDef12 :: () -> Identifier -> Identifier -> ((),TypDefs)
dDef12 _ _ _ = ((),[]) --yet to be implemented

dDef2 :: () -> Modifier -> VarType -> Identifier -> ((), TypDefs)
dDef2 _ _ _ _ = ((),[]) --yet to be implemented

dDef31 :: () -> Identifier -> Stats -> Identifier -> ((), TypDefs)
dDef31 _ _ _ _ = ((),[]) --yet to be implemented

dDef32 :: () -> Identifier -> Identifier -> ((),TypDefs)
dDef32 _ _ _ = ((),[]) --yet to be implemented

dStatBlock :: () -> [Stats] -> ((), Stats)
dStatBlock _ stats = ((), concat stats)

dStatDecl :: () -> Vars -> ((),Stats)
dStatDecl _ vars = ((), vars)

dStatExp :: () -> Exps -> ((), Stats)
dStatExp _ exps = ((),exps)

dStatRet :: () -> Exps -> ((), Stats)
dStatRet _ ret = ((),ret ++ [RET])

dStatIfEl :: () -> Exps -> Stats -> Stats -> ((),Stats)
dStatIfEl _ exp stat1 [] = ((),exp ++ [JUMP2 2] ++ [PUSH 1] ++ [JUMP2 l1] ++ stat1) --yet to be implemented
    where
        l1 = length stat1
dStatIfEl _ exp stat1 stat2 = ((),exp ++ [JUMP2 2] ++ [PUSH 1] ++ [JUMP2 (l1 + 2)] ++ stat1 ++ [JUMP2 (l2 + 1)] ++ stat2) --yet to be implemented
    where
        l1 = length stat1
        l2 = length stat2

dStatWhi :: () -> Exps -> Stats -> ((),Stats)
dStatWhi _ _ _ = ((),[]) --yet to be implemented

dBinExp :: () -> Operator -> Exps -> Exps -> ((),Exps)
dBinExp _ Assignment [LOAD x] exps2 = ((),exps2 ++ [SET x])
dBinExp _ op exps1 exps2 = ((), exps1 ++ exps2 ++ [opToOp op])

dUnaExp1 :: () -> Exps -> Operator -> ((),Exps)
dUnaExp1 _ _ _ = ((),[]) --yet to be implemented

dUnaExp2 :: () -> Operator -> Exps -> ((),Exps)
dUnaExp2 _ _ _ = ((),[]) --yet to be implemented

dFuncCall :: () -> Identifier -> [Exps] -> ((), Exps)
dFuncCall _ id exps = ((), concat (reverse exps)++[JUMP1 id])

dLitInt :: () -> Int -> ((),Exps)
dLitInt _ int = ((),[PUSH int])

dLitChar :: () -> Char -> ((),Exps)
dLitChar _ _ = ((),[]) --yet to be implemented

dLitDouble :: () -> Double -> ((),Exps)
dLitDouble _ _ = ((),[]) --yet to be implemented

dLitVar :: () -> Identifier -> ((),Exps)
dLitVar _ id = ((),[LOAD id])

dLitPointer :: () -> VarType -> Identifier -> ((),Exps)
dLitPointer _ _ _ = ((),[]) --yet to be implemented

dLitArray :: () -> Identifier -> Int -> ((),Exps)
dLitArray _ _ _ = ((),[]) --yet to be implemented

dVar :: () -> Modifier -> VarType -> Identifier -> ((), Vars)
dVar _ _ _ id = ((),[DEFINE id])

dArrayVar :: () -> Modifier -> VarType -> Identifier -> Int -> ((), Vars)
dArrayVar _ _ _ _ _ = ((),[]) --yet to be implemented

dEnumVar :: () -> VarType -> Identifier -> ((), Vars)
dEnumVar _ _ _ = ((),[]) --yet to be implemented

dStructVar :: () -> VarType -> Identifier -> ((),Vars)
dStructVar _ _ _ = ((), []) --yet to be implemented

m :: x -> ((),x)
m x = ((),x)

opToOp :: Operator -> Opcodes
opToOp Add = ADD
opToOp Min = SUB
opToOp Mul = MUL
opToOp Div = DIV
opToOp LessThan = LESS
opToOp GreaterThan = MORE
opToOp EqualComp = EQUAL
opToOp LogicalAnd = AND
opToOp LogicalOr = OR
opToOp LogicalNot = NOT