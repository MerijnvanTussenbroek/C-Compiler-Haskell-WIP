module Library.EnvironmentLibrary where
import AbstractSyntax


-- Desugarying environment

{--
In the desugaring environment we need to keep track of enums, typedefs
and func and var names and types. The names and types must be kept track of for the scope and type checker
It'll make that just a bit easier.
--}

data DesEnv = DesEnv [Enumerator] [TypeDef] [(Identifier, Int, Modifier, VarType)] Int

addEnum :: DesEnv -> Enumerator -> DesEnv
addEnum (DesEnv enumList x y z) enum = DesEnv (enum:enumList) x y z

addTypeDef :: DesEnv -> TypeDef -> DesEnv
addTypeDef (DesEnv x typedefList y z) typedef = DesEnv x (typedef:typedefList) y z

addVariable :: DesEnv -> (Identifier, Int, Modifier, VarType) -> DesEnv
addVariable (DesEnv x y varList z) var = DesEnv x y (var:varList) z

-- checker environment



-- code gen environment

data CodeEnd = CodeEnv 