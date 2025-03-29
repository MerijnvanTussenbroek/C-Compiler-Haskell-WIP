{-# LANGUAGE RecordWildCards #-}
module Algebra where

import AbstractSyntax

data CAlgebra p me s e v env = CAlgebra {
    program :: env -> me -> (p,env),

    memberblock :: env -> [me] -> (me,env),
    memberdeclaration :: env -> v -> (me,env),
    memberstatement :: env -> s -> (me,env),
    memberfunction :: env -> VarType -> String -> [v] -> s -> (me,env),

    statementblock :: env -> [s] -> (s,env),
    statementdeclaration :: env -> v -> (s,env),
    statementexpression :: env -> e -> (s,env),
    statementreturn :: env -> e -> (s,env),
    statementifelse :: env -> e -> s -> s -> (s,env),
    statementwhile :: env -> e -> s -> (s,env),

    binaryexp :: env -> Operator -> e -> e -> (e,env),
    unaryexp :: env -> e -> Operator -> (e,env),
    unaryexp2 :: env ->  Operator -> e -> (e,env),
    funccall :: env -> String -> [e] -> (e,env),
    litint :: env -> Int -> (e,env),
    litchar :: env -> Char -> (e,env),
    litdouble :: env -> Double -> (e,env),
    litvar :: env -> String -> (e,env),
    litArray :: env -> String -> Int -> (e,env),

    var :: env -> Modifier -> VarType -> String -> (v,env),
    array :: env -> Modifier -> VarType -> String -> Int -> (v,env)
    }

cFolder :: CAlgebra p me s e v env-> env -> Program -> (p,env)
cFolder CAlgebra{..} = pfold where

        pfold env (Program m) = let (finalm, finalenv) = mfold env m in program finalenv finalm


        mfold env (MemberBlock m) =     let (finalm, finalenv) = g env m []
                                            g env (x:xs) stepm1 = let (stepm2, stepenv) = mfold env x in g stepenv xs (stepm2:stepm1)
                                            g env [] stepm = (stepm, env)
                                        in memberblock finalenv (reverse finalm)

        mfold env (MemberDeclaration v) = let (outputv,outputenv) = vfold env v in memberdeclaration outputenv outputv

        mfold env (MemberStatement s) = let (finals, finalenv) = sfold env s in memberstatement finalenv finals

        mfold env (MemberFunction vt string v s) =  let (finalenv,finalv,finals) = g env v [] s
                                                        g env (x:xs) stepv1 s = let(stepv2, stepenv) = vfold env x in g stepenv xs (stepv2:stepv1) s
                                                        g env [] stepv s = f env s stepv
                                                        f env s stepv = let (steps,stepenv) = sfold env s in (stepenv,stepv,steps)
                                                    in memberfunction finalenv vt string finalv finals
            

        sfold env (StatementBlock s) =  let (finals, finalenv) = g env s []
                                            g env (x:xs) steps1 = let (steps2, stepenv) = sfold env x in g stepenv xs (steps2 : steps1)
                                            g env [] steps = (steps,env)
                                        in statementblock finalenv (reverse finals)

        sfold env (StatementDeclaration v) = let (finalv, finalenv) = vfold env v in statementdeclaration finalenv finalv

        sfold env (StatementExpression e) = statementexpression finalenv finale
            where
                (finale, finalenv) = efold env e

        sfold env (StatementReturn e) = statementreturn finalenv finale
            where
                (finale, finalenv) = efold env e

        sfold env (StatementIfElse e s1 s2) = statementifelse env finale steps finals
            where
                (finale,nullenv) = efold env e
                (steps,stepenv) = sfold env s1
                (finals,finalenv) = sfold env s2

        sfold env (StatementWhile e s) = let (finals, finalenv) = sfold env s in statementwhile finalenv finale finals
            where
                (finale,nullenv) = efold env e


        efold env (BinaryExp o e1 e2) = binaryexp finalenv o stepe finale
            where
                (stepe, stepenv) = efold env e1
                (finale, finalenv) = efold stepenv e2

        efold env (UnaryExpression e o) = unaryexp finalenv finale o
            where
                (finale,finalenv) = efold env e
        efold env (UnaryExpression2 o e) = unaryexp2 finalenv o finale
            where
                (finale, finalenv) = efold env e

        efold env (FuncCall string e) = funccall finalenv string finale
            where
                g env (x:xs) step = let (stepe,stepenv) = efold env x in g stepenv xs (stepe:step)
                g env [] step = (step,env)
                (finale,finalenv) = g env e []

        efold env (LitInt int) = litint env int

        efold env (LitChar char) = litchar env char

        efold env (LitDouble double) = litdouble env double

        efold env (LitVar string) = litvar env string

        efold env (LitArray string int) = litArray env string int


        vfold env (Var mo vt string) = var env mo vt string

        vfold env (ArrayVar mo vt string int) = array env mo vt string int