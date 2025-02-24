module AnalysisAlgebra where

import AbstractSyntax
import Algebra

analyse = cFolder analysysAlgebra

data AnalysisErrors = NoErrors
                        | ScopeError String
                        | TypeError String
                        | ScopeAndTypeError String
    deriving Eq

instance Show AnalysisErrors where
    show NoErrors = "No errors"
    show (ScopeError x) = "Scope error detected: " ++ x
    show (TypeError x) = "Type error detected: " ++ x
    show (ScopeAndTypeError x) = "Scope and Type errors detected: " ++ x

analysysAlgebra :: CAlgebra AnalysisErrors me s exp v [(String,VarType)]
analysysAlgebra = CAlgebra
    foldOverProgram -- program

    foldOverMemberBlock -- memberblock
    foldOverMemberDeclaration -- memberdeclaration
    foldOverMemberStatement -- memberstatement
    foldOverMemberFunction -- memberfunction

    foldOverStatementBlock -- statementblock
    foldOverStatementDeclaration -- statementdeclaration
    foldOverStatementExpression -- statementexpression
    foldOverStatementReturn -- statementreturn
    foldOverStatementIfElse -- statementifelse
    foldOverStatementWhile -- statementwhile

    foldOverBinaryExpr -- binaryexp
    foldOverUnaryExpr -- unaryexp
    foldOverFuncCall -- funccall
    foldOverLitInt -- litint
    foldOverLitChar -- litchar
    foldOverLitDouble -- litdouble
    foldOverLitVar -- litvar
    foldOverLitBool
    foldOverLitArray -- litarray

    foldOverVar -- var
    foldOverArrayVar -- arrayVar

foldOverProgram :: env -> me -> (p,env)
foldOverProgram = undefined 

foldOverMemberBlock :: env -> [me] -> (me,env)
foldOverMemberBlock = undefined

foldOverMemberDeclaration :: env -> v -> (me,env)
foldOverMemberDeclaration = undefined

foldOverMemberStatement :: env -> s -> (me,env)
foldOverMemberStatement = undefined

foldOverMemberFunction :: env -> VarType -> String -> [v] -> s -> (me,env)
foldOverMemberFunction = undefined

foldOverStatementBlock :: env -> [s] -> (s,env)
foldOverStatementBlock = undefined

foldOverStatementDeclaration :: env -> v -> (s,env)
foldOverStatementDeclaration = undefined

foldOverStatementExpression :: env -> exp -> (s,env)
foldOverStatementExpression = undefined

foldOverStatementReturn :: env -> exp -> (s,env)
foldOverStatementReturn = undefined

foldOverStatementIfElse :: env -> exp -> s -> s -> (s,env)
foldOverStatementIfElse = undefined

foldOverStatementWhile :: env -> exp -> s -> (s,env)
foldOverStatementWhile = undefined

foldOverBinaryExpr :: env -> Operator -> exp -> exp -> (exp,env)
foldOverBinaryExpr env Assign e1 e2 = undefined
foldOverBinaryExpr env op e1 e2 = undefined

foldOverUnaryExpr :: env -> exp -> Operator -> (exp,env)
foldOverUnaryExpr = undefined

foldOverFuncCall :: env -> String -> [exp] -> (exp,env)
foldOverFuncCall env "printf" xs = undefined
foldOverFuncCall env name xs = undefined

foldOverLitInt :: env -> Int -> (exp,env)
foldOverLitInt = undefined

foldOverLitChar :: env -> Char -> (exp,env)
foldOverLitChar = undefined

foldOverLitDouble :: env -> Double -> (exp,env)
foldOverLitDouble = undefined

foldOverLitVar :: env -> String -> (exp,env)
foldOverLitVar = undefined

foldOverLitBool :: env -> Bool -> (exp,env)
foldOverLitBool = undefined

foldOverLitArray :: env -> String -> Int -> (exp,env)
foldOverLitArray = undefined

foldOverVar :: env -> Modifier -> VarType -> String -> (v,env)
foldOverVar = undefined

foldOverArrayVar :: env -> Modifier -> VarType -> String -> Int -> (v,env)
foldOverArrayVar = undefined