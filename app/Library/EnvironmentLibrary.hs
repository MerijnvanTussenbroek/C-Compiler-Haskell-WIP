module Library.EnvironmentLibrary where
import AbstractSyntax
import Data.Maybe (isNothing)


-- Desugarying environment

{--
In the desugaring environment we need to keep track of enums, typedefs
and func and var names and types. The names and types must be kept track of for the scope and type checker
It'll make that just a bit easier.
--}

data DesEnv = DesEnv [Enumerator] [TypeDef] 
    deriving Show

addEnum :: DesEnv -> Enumerator -> DesEnv
addEnum (DesEnv enumList x) enum = DesEnv (enum:enumList) x

addTypeDef :: DesEnv -> TypeDef -> DesEnv
addTypeDef (DesEnv x typedefList) typedef = DesEnv x (typedef:typedefList)

searchForVariable :: DesEnv -> Expression -> Expression
searchForVariable (DesEnv (x:xs) y) (LitVar id) | compareResult result (LitVar id) = searchForVariable (DesEnv xs y) (LitVar id)
                                                    | otherwise = result
    where
        result = searchEnum x id (LitInt 0)
searchForVariable (DesEnv [] y) (LitVar id) = LitVar id

compareResult :: Expression -> Expression -> Bool
compareResult (LitVar id) (LitVar id2) = id == id2
compareResult _ (LitVar id) = False;

searchEnum :: Enumerator -> Identifier -> Expression -> Expression
searchEnum (Enum enumID (StatementBlock ((StatementExpression (BinaryExp Assignment (LitVar name) exp)):xs))) id int| name == id = exp
                                                                                                                    | otherwise = searchEnum (Enum enumID (StatementBlock  xs)) id (BinaryExp Add exp (LitInt 1))
searchEnum (Enum enumID (StatementBlock ((StatementExpression (LitVar name)):xs))) id int   | name == id = int
                                                                                            | otherwise = searchEnum (Enum enumID (StatementBlock  xs)) id (BinaryExp Add int (LitInt 1))
searchEnum (Enum enumID (StatementBlock [])) id int = LitVar id

searchTypedefs :: Identifier -> Identifier -> [TypeDef] -> Maybe Variable
searchTypedefs acid x ((Def1 (Struct ids vars) id):ys)      | x == id = Just (StructVar (SelfDefined ids) acid)
                                                            | otherwise = searchTypedefs acid x ys
searchTypedefs acid x ((Def1 (StructTypedef ids) id):ys)    | x == id = Just (StructVar (SelfDefined ids) acid)
                                                            | otherwise = searchTypedefs acid x ys
searchTypedefs acid x ((Def2 mod vt id):ys) | x == id = Just (Var mod vt acid)
                                            | otherwise = searchTypedefs acid x ys
searchTypedefs acid x ((Def3 enum id):ys)   | x == id = Just (Var None IntType acid)
                                            | otherwise = searchTypedefs acid x ys
searchTypedefs acid x [] = Nothing

isEnum :: Identifier -> [Enumerator] -> Bool
isEnum x ((Enum id st):ys)  | x == id = True
                            | otherwise = isEnum x ys
isEnum x [] = False;

isTypedef :: Identifier -> [TypeDef] -> Bool
isTypedef x ((Def1 s id):ys)    | x == id = True
                                | otherwise = isTypedef x ys
isTypedef x ((Def2 mod vt id):ys)   | x == id = True
                                    | otherwise = isTypedef x ys
isTypedef x ((Def3 enum id):ys) | x == id = True
                                | otherwise = isTypedef x ys
isTypedef _ [] = False

-- checker environment

data CheckEnv = CheckEnv [(Identifier, VarType)] [(Identifier, VarType)] [Identifier] [Identifier]

-- variables, functions, scope, type

cAddVar :: CheckEnv -> Variable -> CheckEnv
cAddVar (CheckEnv vars mems vp fp) (Var mod vt id) = CheckEnv ((id, vt):vars) mems vp fp
cAddVar (CheckEnv vars mems vp fp) (ArrayVar mod vt id int) = CheckEnv ((id, vt):vars) mems vp fp
cAddVar (CheckEnv vars mems vp fp) (EnumVar vt id) = CheckEnv ((id, vt):vars) mems vp fp
cAddVar (CheckEnv vars mems vp fp) (StructVar vt id) = CheckEnv ((id, vt):vars) mems vp fp

cAddFunc :: CheckEnv -> Members -> CheckEnv
cAddFunc (CheckEnv vars1 mems vp fp) (MemberFunction vt id vars2 stat) = CheckEnv vars1 ((id, vt):mems) vp fp
cAddFunc (CheckEnv vars1 mems vp fp) _ = CheckEnv vars1 mems vp fp

cCheckerVar :: CheckEnv -> Variable -> Bool
cCheckerVar (CheckEnv vars mems vp fp) (Var mod vt id) = checker vars (id, vt)
cCheckerVar (CheckEnv vars mems vp fp) (ArrayVar mod vt id int) = checker vars (id, vt)
cCheckerVar (CheckEnv vars mems vp fp) (EnumVar vt id) = checker vars (id, vt)
cCheckerVar (CheckEnv vars mems vp fp) (StructVar vt id) = checker vars (id, vt)

cCheckerFunc :: CheckEnv -> Members -> Bool
cCheckerFunc (CheckEnv vars1 mems vp fp) (MemberFunction vt id vars2 stat) = checker vars1 (id, vt)
cCheckerFunc (CheckEnv vars1 mems vp fp) _ = False

checker :: [(Identifier, VarType)] -> (Identifier, VarType) -> Bool
checker ((id1, vt1):xs) alg@(id2, vt2)  | id1 == id2 = vt1 == vt2
                                        | otherwise = checker xs alg
checker [] _ = False

getVarType :: CheckEnv -> Identifier -> VarType
getVarType (CheckEnv ((id1, vt1):xs) mems vp fp) id2    | id1 == id2 = vt1
                                                        | otherwise = getVarType (CheckEnv xs mems vp fp) id2
getVarType (CheckEnv [] mems vp fp) id = SelfDefined id

getFuncType :: CheckEnv -> Identifier -> VarType
getFuncType (CheckEnv vars1 ((id1, vt1):xs) vp fp) id2  | id1 == id2 = vt1
                                                        | otherwise = getFuncType (CheckEnv vars1 xs vp fp) id2
getFuncType (CheckEnv vars1 [] vp fp) id = SelfDefined id

typeProblem :: CheckEnv -> Identifier -> CheckEnv
typeProblem (CheckEnv vars mems vp fp) id = CheckEnv vars mems vp (id:fp)

scopeProblem :: CheckEnv -> Identifier -> CheckEnv
scopeProblem (CheckEnv vars mems vp fp) id = CheckEnv vars mems (id:vp) fp

