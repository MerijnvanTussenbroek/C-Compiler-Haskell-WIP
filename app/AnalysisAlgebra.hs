{-# LANGUAGE InstanceSigs #-}
module AnalysisAlgebra where

import AbstractSyntax
import Algebra
import Library.EnvironmentLibrary

analyse = cFolder analysisAlgebra (CheckEnv [] [] [] [])

data AnalysisErrors = NoErrors
                        | ScopeError String
                        | TypeError String
                        | ScopeAndTypeError String String

instance Show AnalysisErrors where
    show :: AnalysisErrors -> String
    show NoErrors = "No errors"
    show (ScopeError x) = "Scope error detected: " ++ x
    show (TypeError x) = "Type error detected: " ++ x
    show (ScopeAndTypeError x y) = "Scope and Type errors detected: " ++ x ++ " " ++ y

analysisAlgebra :: CAlgebra AnalysisErrors () () (Identifier, VarType) () CheckEnv () () ()
analysisAlgebra = CAlgebra
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
    dUnumVar 
    dStructVar 

dProgram alg@(CheckEnv vars funcs [] []) me = (alg, NoErrors)
dProgram alg@(CheckEnv vars funcs vp []) me = (alg, ScopeError (unwords vp))
dProgram alg@(CheckEnv vars funcs [] fp) me = (alg, TypeError (unwords fp))
dProgram alg@(CheckEnv vars funcs vp fp) me = (alg, ScopeAndTypeError (unwords vp) (unwords fp))

dMemberBlock env me = (env, ())
dMemberDecl env v = (env, ())
dMemberStat env s = (env, ())
dMemberFunc env vt id v s = (env, ())
dMemberEnum env en = (env, ())
dMemberStruct env st = (env, ())
dMemberInc env str = (env, ())
dMemberTypedef env typ = (env, ())

dEnum env id s = (env, ())

dStruct env id1 v = (env, ())

dDef11 env id1 v id2 = (env, ())
dDef12 env id1 id2 = (env, ())
dDef2 env mod vt id = (env, ())
dDef31 env id1 s id2 = (env, ())
dDef32 env id1 id2 = (env, ())

dStatBlock env s = (env, ())
dStatDecl env v = (env, ())
dStatExp env e = (env, ())
dStatRet env e = (env, ())
dStatIfEl env e s1 s2 = (env, ())
dStatWhi env e s = (env, ())



dBinExp env op alg1@([], type1) alg2@([], type2)    | type1 == type2 = (env, ([], type2))
                                                    | otherwise = helperfunc env alg1 alg2
dBinExp env op alg1@(id1, type1) alg2@([], type2)   | type1 == type2 = (env, alg1)
                                                    | otherwise = helperfunc env alg1 alg2
dBinExp env op alg1@([], type1) alg2@(id2, type2)   | type1 == type2 = (env, alg2)
                                                    | otherwise = helperfunc env alg1 alg2
dBinExp env op alg1@(id1, type1) alg2@(id2, type2)  | type1 == type2 = (env, (id2, type2))
                                                    | otherwise = helperfunc env alg1 alg2

helperfunc :: CheckEnv -> (Identifier, VarType) -> (Identifier, VarType) -> (CheckEnv, (Identifier, VarType))
helperfunc env (id1, SelfDefined x) (id2, type2) = (typeProblem env id1, ([], type2))
helperfunc env (id1, type1) (id2, SelfDefined x) = (typeProblem env id2, ([], type1))
helperfunc env (id1, IntType) (id2, DoubleType) = (typeProblem env id2, ([], IntType))
helperfunc env (id1, DoubleType) (id2, IntType) = (typeProblem env id1, ([], IntType))
helperfunc env (id1, CharType) (id2, DoubleType) = (typeProblem env id2, ([], CharType))
helperfunc env (id1, DoubleType) (id2, CharType) = (typeProblem env id1, ([], CharType))
helperfunc env (id1, type1) (id2, type2) = (typeProblem env (id1 ++ " " ++ id2),([], type1))
dUnaExp1 env e op = (env, e)
dUnaExp2 env op e = (env, e)
dFuncCall :: CheckEnv -> [Char] -> p -> (CheckEnv, ([Char], VarType))
dFuncCall env id exps = (env, (id, getFuncType env id))
dLitInt :: a1 -> p -> (a1, ([a2], VarType))
dLitInt env _ = (env, ([], IntType))
dLitChar :: a1 -> p -> (a1, ([a2], VarType))
dLitChar env _ = (env, ([], CharType))
dLitDouble :: a1 -> p -> (a1, ([a2], VarType))
dLitDouble env _ = (env, ([], DoubleType))
dLitVar :: CheckEnv -> [Char] -> (CheckEnv, ([Char], VarType))
dLitVar env id = (env, (id, getVarType env id))
dLitPointer :: CheckEnv -> p -> [Char] -> (CheckEnv, ([Char], VarType))
dLitPointer env vt id = (env, (id, getVarType env id))
dLitArray :: CheckEnv -> [Char] -> p -> (CheckEnv, ([Char], VarType))
dLitArray env id int = (env, (id, getVarType env id))

dVar :: CheckEnv -> Modifier -> VarType -> Identifier -> (CheckEnv, ())
dVar env mod vt@(SelfDefined x) id = (cAddVar env (Var mod vt id), ())
dVar env mod vt id = (cAddVar env (Var mod vt id), ())
dArrayVar :: CheckEnv -> Modifier -> VarType -> Identifier -> Int -> (CheckEnv, ())
dArrayVar env mod vt@(SelfDefined x) id int = (cAddVar env (ArrayVar mod vt id int), ())
dArrayVar env mod vt id int = (cAddVar env (ArrayVar mod vt id int), ())
dUnumVar :: CheckEnv -> VarType -> Identifier -> (CheckEnv, ())
dUnumVar env vt@(SelfDefined x) id = (cAddVar env (EnumVar vt id), ())
dUnumVar env vt id = (cAddVar env (EnumVar vt id), ())
dStructVar :: CheckEnv -> VarType -> Identifier -> (CheckEnv, ())
dStructVar env vt@(SelfDefined x) id = (cAddVar env (StructVar vt id), ())
dStructVar env vt id = (cAddVar env (StructVar vt id), ())
