module Desugering where

import AbstractSyntax
import Algebra

-- this here is for desugering the datastructure before we start folding over it
-- this way, I don't have to add semantics for all possible operators

desugerar = cFolder desugeringAlgebra ()

desugeringAlgebra :: CAlgebra Program Members Statements Expression Variable ()
desugeringAlgebra = CAlgebra
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
    foldOverLitBool -- litbool
    foldOverLitArray --litarray

    foldOverVar -- var
    foldOverArrayVar -- arrayvar

foldOverProgram :: () -> Members -> (Program,())
foldOverProgram env me = (Program me,env) 

foldOverMemberBlock :: () -> [Members] -> (Members,())
foldOverMemberBlock env me = (MemberBlock me, env)

foldOverMemberDeclaration :: () -> Variable -> (Members,())
foldOverMemberDeclaration env var = (MemberDeclaration var, env)

foldOverMemberStatement :: () -> Statements -> (Members,())
foldOverMemberStatement env stat = (MemberStatement stat, env)

foldOverMemberFunction :: () -> VarType -> String -> [Variable] -> Statements -> (Members,())
foldOverMemberFunction env vtype name vars stat = (MemberFunction vtype name vars stat, env)

foldOverStatementBlock :: () -> [Statements] -> (Statements,())
foldOverStatementBlock env stats = (StatementBlock stats, env)

foldOverStatementDeclaration :: () -> Variable -> (Statements,())
foldOverStatementDeclaration env var = (StatementDeclaration var, env)

foldOverStatementExpression :: () -> Expression -> (Statements,())
foldOverStatementExpression env exp = (StatementExpression exp, env)

foldOverStatementReturn :: () -> Expression -> (Statements,())
foldOverStatementReturn env exp = (StatementReturn exp, env)

foldOverStatementIfElse :: () -> Expression -> Statements -> Statements -> (Statements,())
foldOverStatementIfElse env exp stat1 stat2 = (StatementIfElse exp stat1 stat2, env)

foldOverStatementWhile :: () -> Expression -> Statements -> (Statements,())
foldOverStatementWhile env exp stat = (StatementWhile exp stat, env)

foldOverBinaryExpr :: () -> Operator -> Expression -> Expression -> (Expression,())
foldOverBinaryExpr x MulAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp Mul e1 e2),x)
foldOverBinaryExpr x DivAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp Div e1 e2),x)
foldOverBinaryExpr x AddAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp Add e1 e2),x)
foldOverBinaryExpr x MinAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp Min e1 e2),x)
foldOverBinaryExpr x ModAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp Mod e1 e2),x)
foldOverBinaryExpr x AndAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp AndOp e1 e2),x)
foldOverBinaryExpr x OrAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp OrOp e1 e2),x)
foldOverBinaryExpr x XorAssign e1 e2 = (BinaryExp Assign e1 (BinaryExp XorOp e1 e2),x)
foldOverBinaryExpr x LessOrEqualTo e1 e2 = (BinaryExp OrOp (BinaryExp LessThan e1 e2) (BinaryExp EqualTo e1 e2),x)
foldOverBinaryExpr x GreaterOrEqualTo e1 e2 = (BinaryExp AndOp (BinaryExp GreaterThan e1 e2) (BinaryExp EqualTo e1 e2),x)
foldOverBinaryExpr x op e1 e2 = (BinaryExp op e1 e2,x)

foldOverUnaryExpr :: () -> Expression -> Operator -> (Expression,())
foldOverUnaryExpr env exp AddOne = (BinaryExp Assign exp (BinaryExp Add exp (LitInt 1)), env)
foldOverUnaryExpr env exp MinusOne = (BinaryExp Assign exp (BinaryExp Min exp (LitInt 1)), env)

foldOverFuncCall :: () -> String -> [Expression] -> (Expression,())
foldOverFuncCall env name xs = (FuncCall name xs, env)

foldOverLitInt :: () -> Int -> (Expression,())
foldOverLitInt env int = (LitInt int, env)

foldOverLitChar :: () -> Char -> (Expression,())
foldOverLitChar env char = (LitChar char, env)

foldOverLitDouble :: () -> Double -> (Expression,())
foldOverLitDouble env double = (LitDouble double, env)

foldOverLitVar :: () -> String -> (Expression,())
foldOverLitVar env name = (LitVar name, env)

foldOverLitBool :: () -> Bool -> (Expression,())
foldOverLitBool env bool = (LitBool bool, env)

foldOverLitArray :: env -> String -> Int -> (Expression,env)
foldOverLitArray env string int = (LitArray string int, env)

foldOverVar :: () -> Modifier -> VarType -> String -> (Variable,())
foldOverVar env mod vtype name = (Var mod vtype name, env)

foldOverArrayVar :: env -> Modifier -> VarType -> String -> Int -> (Variable,env)
foldOverArrayVar env mo vart string int = (ArrayVar mo vart string int, env)