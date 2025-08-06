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

data CheckEnv = CheckEnv [Variable] [Members] [TypeDef] [Enumerator]
