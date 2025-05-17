{-# LANGUAGE RecordWildCards #-}
module Algebra where

import AbstractSyntax

data CAlgebra p me s e v env en st inc typ = CAlgebra {
    program :: me -> p,

    memberBlock :: [me] -> p,
    memberDecl :: v -> me,
    memberStat :: s -> me,
    memberFunc :: VarType -> Identifier -> [v] -> s -> me,
    memberEnum :: en -> me,
    memberStruct :: st -> me,
    memberInc :: String -> me,
    memberTypedef :: typ -> me,

    enum :: Identifier -> s -> en,
    enumTypedef :: Identifier -> en,

    struct :: Identifier -> [v] -> st,
    structTypedef :: Identifier -> st,

    def1 :: st -> Identifier -> typ,
    def2 :: Modifier -> VarType -> Identifier -> typ,
    def3 :: en -> Identifier -> typ,

    statBlock :: [s] -> s,
    statDecl :: v -> s,
    statExp :: e -> s,
    statRet :: e -> s,
    statIfEl :: e -> s -> s -> s,
    statWhi :: e -> s -> s,

    binExp :: Operator -> e -> e -> e,
    unaExp1 :: e -> Operator -> e,
    unaExp2 :: Operator -> e -> e,
    funcCall :: Identifier -> [e] -> e,
    litInt :: Int -> e,
    litChar :: Char -> e,
    litDouble :: Double -> e,
    litVar :: Identifier -> e,
    litPointer :: VarType -> Identifier -> e,

    var :: Modifier -> VarType -> Identifier -> v,
    arrayVar :: Modifier -> VarType -> Identifier -> Int -> v,
    structVar :: VarType -> Identifier -> v
    }

cFolder :: CAlgebra p me s e v env en st inc typ -> env -> Program -> (p,env)
cFolder CAlgebra{..} = undefined

        