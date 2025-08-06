module Desugering where

import AbstractSyntax
import Algebra
import Library.EnvironmentLibrary

import Data.Maybe
import Debug.Trace

-- this here is for desugering the datastructure before we start folding over it
-- this way, I don't have to add semantics for all possible operators


desugerar = cFolder desugeringAlgebra (DesEnv [] [])

desugeringAlgebra :: CAlgebra Program Members Statements Expression Variable DesEnv Enumerator Struct TypeDef
desugeringAlgebra = CAlgebra
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

dProgram :: DesEnv -> Members -> (DesEnv, Program)
dProgram desenv members = (desenv, Program members)

dMemberBlock :: DesEnv -> [Members] -> (DesEnv, Members)
dMemberBlock desenv members = (desenv, MemberBlock members)

dMemberDecl :: DesEnv -> Variable -> (DesEnv, Members)
dMemberDecl desenv var = (desenv, MemberDeclaration var)

dMemberStat :: DesEnv -> Statements -> (DesEnv, Members)
dMemberStat desenv stat = (desenv, MemberStatement stat)

dMemberFunc :: DesEnv -> VarType -> Identifier -> [Variable] -> Statements -> (DesEnv, Members)
dMemberFunc desenv vart id vars stats = (desenv, MemberFunction vart id vars stats)

dMemberEnum :: DesEnv -> Enumerator -> (DesEnv, Members)
dMemberEnum desenv enum = (addEnum desenv enum, MemberBlock [])

dMemberStruct :: DesEnv -> Struct -> (DesEnv, Members)
dMemberStruct desenv str = (desenv, MemberBlock []) -- yet to be implemented

dMemberInc :: DesEnv -> String -> (DesEnv, Members)
dMemberInc desenv name = (desenv, MemberBlock [])

dMemberTypedef :: DesEnv -> TypeDef -> (DesEnv, Members)
dMemberTypedef desenv typ = (addTypeDef desenv typ, MemberBlock [])

dEnum :: DesEnv -> Identifier -> Statements -> (DesEnv, Enumerator)
dEnum desenv id stats = (desenv, Enum id stats)

dStruct :: DesEnv -> Identifier -> [Variable] -> (DesEnv, Struct)
dStruct desenv id vars = (desenv, Struct id vars)

dDef11 :: DesEnv -> Identifier -> [Variable] -> Identifier -> (DesEnv, TypeDef)
dDef11 desenv id1 vars id2 = (desenv, Def1 (Struct id1 vars) id2)

dDef12 :: DesEnv -> Identifier -> Identifier -> (DesEnv, TypeDef)
dDef12 desenv id1 id2 = (desenv, Def1 (StructTypedef id1) id2)

dDef2 :: DesEnv -> Modifier -> VarType -> Identifier -> (DesEnv, TypeDef)
dDef2 desenv mod vart id = (desenv, Def2 mod vart id)

dDef31 :: DesEnv -> Identifier -> Statements -> Identifier -> (DesEnv, TypeDef)
dDef31 desenv id1 stats id2 = (desenv, Def3 (Enum id1 stats) id2)

dDef32 :: DesEnv -> Identifier -> Identifier -> (DesEnv, TypeDef)
dDef32 desenv id1 id2 = (desenv, Def3 (EnumTypedef id1) id2)

dStatBlock :: DesEnv -> [Statements] -> (DesEnv, Statements)
dStatBlock desenv stats = (desenv, StatementBlock stats)

dStatDecl :: DesEnv -> Variable -> (DesEnv, Statements)
dStatDecl desenv var = (desenv, StatementDeclaration var)

dStatExp :: DesEnv -> Expression -> (DesEnv, Statements)
dStatExp desenv exp = (desenv, StatementExpression exp)

dStatRet :: DesEnv -> Expression -> (DesEnv, Statements)
dStatRet desenv exp = (desenv, StatementReturn exp)

dStatIfEl :: DesEnv -> Expression -> Statements -> Statements -> (DesEnv, Statements)
dStatIfEl desenv exp st1 st2 = (desenv, StatementIfElse exp st1 st2)

dStatWhi :: DesEnv -> Expression -> Statements -> (DesEnv, Statements)
dStatWhi desenv exp st = (desenv, StatementWhile exp st)

dBinExp :: DesEnv -> Operator -> Expression -> Expression -> (DesEnv, Expression)
dBinExp desenv op exp1 exp2 = (desenv, BinaryExp op exp1 exp2)

dUnaExp1 :: DesEnv -> Expression -> Operator -> (DesEnv, Expression)
dUnaExp1 desenv exp1 op = (desenv, UnaryExpression exp1 op)

dUnaExp2 :: DesEnv -> Operator -> Expression -> (DesEnv, Expression)
dUnaExp2 desenv AddOne exp1 = (desenv, BinaryExp Assignment exp1 (BinaryExp Add exp1 (LitInt 1)))
dUnaExp2 desenv MinOne exp1 = (desenv, BinaryExp Assignment exp1 (BinaryExp Min exp1 (LitInt 1)))
dUnaExp2 desenv op exp1 = (desenv, UnaryExpression2 op exp1)

dFuncCall :: DesEnv -> Identifier -> [Expression] -> (DesEnv, Expression)
dFuncCall desenv id exps = (desenv, FuncCall id exps)

dLitInt :: DesEnv -> Int -> (DesEnv, Expression)
dLitInt desenv int = (desenv, LitInt int)

dLitChar :: DesEnv -> Char -> (DesEnv, Expression)
dLitChar desenv char = (desenv, LitChar char)

dLitDouble :: DesEnv -> Double -> (DesEnv, Expression)
dLitDouble desenv double = (desenv, LitDouble double)

dLitVar :: DesEnv -> Identifier -> (DesEnv, Expression)
dLitVar desenv id = (desenv, searchForVariable desenv (LitVar id))

dLitPointer :: DesEnv -> VarType -> Identifier -> (DesEnv, Expression)
dLitPointer desenv vart id = (desenv, LitPointer vart id)

dLitArray :: DesEnv -> Identifier -> Int -> (DesEnv, Expression)
dLitArray desenv id int = (desenv, LitArray id int)

dVar :: DesEnv -> Modifier -> VarType -> Identifier -> (DesEnv, Variable)
dVar desenv@(DesEnv enums deffs) mod (SelfDefined x) id | isNothing result1 = (desenv, Var mod (SelfDefined x) id)  
                                                        | otherwise = (desenv, fromJust result1)
    where
        result1 = searchTypedefs id x deffs
dVar desenv mod vart id = (desenv, Var mod vart id)

dArrayVar :: DesEnv -> Modifier -> VarType -> Identifier -> Int -> (DesEnv, Variable)
dArrayVar  desenv mod vart id int = (desenv, ArrayVar mod vart id int)

dEnumVar :: DesEnv -> VarType -> Identifier -> (DesEnv, Variable)
dEnumVar desenv vart id = (desenv, Var None IntType id)

dStructVar :: DesEnv -> VarType -> Identifier -> (DesEnv, Variable)
dStructVar desenv vart id = (desenv, StructVar vart id)