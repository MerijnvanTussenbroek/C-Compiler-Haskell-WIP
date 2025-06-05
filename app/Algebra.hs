{-# LANGUAGE RecordWildCards #-}
module Algebra where

import AbstractSyntax

data CAlgebra p me s e v env en st typ = CAlgebra {
    program :: env -> me -> (env,p),

    memberBlock :: env -> [me] -> (env,me),
    memberDecl :: env -> v -> (env,me),
    memberStat :: env -> s -> (env,me),
    memberFunc :: env -> VarType -> Identifier -> [v] -> s -> (env,me),
    memberEnum :: env -> en -> (env,me),
    memberStruct :: env -> st -> (env,me),
    memberInc :: env -> String -> (env,me),
    memberTypedef :: env -> typ -> (env,me),

    enum :: env -> Identifier -> s -> (env,en),

    struct :: env -> Identifier -> [v] -> (env,st),

    def11 :: env -> Identifier -> [v] -> Identifier -> (env,typ),
    def12 :: env -> Identifier -> Identifier -> (env,typ),
    def2 :: env -> Modifier -> VarType -> Identifier -> (env,typ),
    def31 :: env -> Identifier -> s -> Identifier -> (env,typ),
    def32 :: env -> Identifier -> Identifier -> (env,typ),

    statBlock :: env -> [s] -> (env,s),
    statDecl :: env -> v -> (env,s),
    statExp :: env -> e -> (env,s),
    statRet :: env -> e -> (env,s),
    statIfEl :: env -> e -> s -> s -> (env,s),
    statWhi :: env -> e -> s -> (env,s),

    binExp :: env -> Operator -> e -> e -> (env,e),
    unaExp1 :: env -> e -> Operator -> (env,e),
    unaExp2 :: env -> Operator -> e -> (env,e),
    funcCall :: env -> Identifier -> [e] -> (env,e),
    litInt :: env -> Int -> (env,e),
    litChar :: env -> Char -> (env,e),
    litDouble :: env -> Double -> (env,e),
    litVar :: env -> Identifier -> (env,e),
    litPointer :: env -> VarType -> Identifier -> (env,e),
    litArray :: env -> Identifier -> Int -> (env, e),

    var :: env -> Modifier -> VarType -> Identifier -> (env,v),
    arrayVar :: env -> Modifier -> VarType -> Identifier -> Int -> (env,v),
    enumVar :: env -> VarType -> Identifier -> (env, v),
    structVar :: env -> VarType -> Identifier -> (env,v)
    }

cFolder :: CAlgebra p me s e v env en st typ -> env -> Program -> (env, p)
cFolder CAlgebra{..} = fold
    where
        fold env (Program p) = program env2 mes
            where
                (env2, mes) = mefold env p

        mefold env1 (MemberBlock mems) = memberBlock env2 sfirst
            where
                g env (x:xs) total = let (tempenv, temps) = mefold env x in g tempenv xs (total++[temps])
                g env [] total = (env, total)
                (env2, sfirst) = g env1 mems []
        mefold env1 (MemberDeclaration v) = memberDecl env2 vfirst
            where
                (env2, vfirst) = vfold env1 v
        mefold env1 (MemberStatement s) = memberStat env2 sfirst
            where
                (env2, sfirst) = sfold env1 s
        mefold env1 (MemberFunction vart id vars stats) = memberFunc env3 vart id vfirst sfirst
            where
                g env (x:xs) total = let (tempenv, tempv) = vfold env x in g tempenv xs (total++[tempv])
                g env [] total = (env, total)
                (env2, vfirst) = g env1 vars []
                (env3, sfirst) = sfold env2 stats
        mefold env1 (MemberEnum enum) = memberEnum env2 enfirst 
            where
                (env2, enfirst) = enfold env1 enum
        mefold env1 (MemberStruct struct) = memberStruct env2 stfirst
            where
                (env2, stfirst) = stfold env1 struct
        mefold env1 (MemberInclude string) = memberInc env1 string
        mefold env1 (MemberTypedef typedef) = memberTypedef env2 tfirst
            where
                (env2, tfirst) = typfold env1 typedef



        enfold env1 (Enum id stats) = enum env2 id ssfirst
            where
                (env2, ssfirst) = sfold env1 stats



        stfold env1 (Struct id vars) = struct env2 id vfirst
            where
                g env (x:xs) total = let (tempenv, tempv) = vfold env x in g tempenv xs (total++[tempv])
                g env [] total = (env, total)
                (env2, vfirst) = g env1 vars []



        typfold env1 (Def1 (Struct stid vars) id) = def11 env2 stid vfirst id
            where
                g env (x:xs) total = let (tempenv, tempv) = vfold env x in g tempenv xs (total++[tempv])
                g env [] total = (env, total)
                (env2, vfirst) = g env1 vars []
        typfold env1 (Def1 (StructTypedef stid) id) = def12 env1 stid id
        typfold env1 (Def2 mod vart id) = def2 env1 mod vart id
        typfold env1 (Def3 (Enum enid st) id) = def31 env2 enid sfirst id
            where
                (env2, sfirst) = sfold env1 st
        typfold env1 (Def3 (EnumTypedef enid) id) = def32 env1 enid id



        sfold env1 (StatementBlock stats) = statBlock env2 sfirst
            where
                g env (x:xs) total = let (tempenv, tempstat) = sfold env x in g tempenv xs (total++[tempstat])
                g env [] total = (env,total)
                (env2, sfirst) = g env1 stats []
        sfold env1 (StatementDeclaration v) = statDecl env2 vfirst
            where
                (env2, vfirst) = vfold env1 v
        sfold env1 (StatementExpression e) = statExp env2 efirst
            where
                (env2, efirst) = efold env1 e
        sfold env1 (StatementReturn e) = statRet env2 efirst
            where
                (env2, efirst) = efold env1 e
        sfold env1 (StatementIfElse e s1 s2) = statIfEl env3 efirst sfirst ssecond
            where
                (env2, efirst) = efold env1 e
                (env3, sfirst) = sfold env2 s1
                (env4, ssecond) = sfold env3 s2
        sfold env1 (StatementWhile e s1) = statWhi env3 efirst sfirst
            where
                (env2, efirst) = efold env1 e
                (env3, sfirst) = sfold env2 s1



        efold env1 (BinaryExp op e1 e2) = binExp env3 op efirst esecond
            where
                (env2, efirst) = efold env1 e1
                (env3, esecond) = efold env2 e2
        efold env1 (UnaryExpression e1 op) = unaExp1 env2 efirst op
            where
                (env2, efirst) = efold env1 e1
        efold env1 (UnaryExpression2 op e1) = unaExp2 env2 op efirst
            where
                (env2, efirst) = efold env1 e1
        efold env1 (FuncCall id eps) = funcCall env2 id exps
            where
                g (x:xs) env exps = let (newEnv, newE) = efold env x in g xs newEnv (exps++[newE])
                g [] env exps = (env, exps)

                (env2, exps) = g eps env1 []
        efold env1 (LitInt int) = litInt env1 int
        efold env1 (LitChar char) = litChar env1 char
        efold env1 (LitDouble double) = litDouble env1 double
        efold env1 (LitVar id) = litVar env1 id
        efold env1 (LitPointer vart id) = litPointer env1 vart id
        efold env1 (LitArray id int) = litArray env1 id int



        vfold env1 (Var mod vart id) = var env1 mod vart id
        vfold env1 (ArrayVar mod vart id int) = arrayVar env1 mod vart id int
        vfold env1 (EnumVar vart id) = enumVar env1 vart id
        vfold env1 (StructVar vart id) = structVar env1 vart id

        