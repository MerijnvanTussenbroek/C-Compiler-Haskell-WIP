module AbstractSyntax where

-- Tokens for the Lexer

data Token  = SpaceToken
            | Comment
            | Semicolon
            | OpeningBracket
            | ClosingBracket
            | OpeningSquareBracket
            | ClosingSquareBracket
            | OpeningRoundBracket
            | ClosingRoundBracket
            | Apostrofy
            | Comma
            | Operator Operator
            | Name String
            | Type VarType
            | Modifier Modifier
            | IntegerVar Int
            | DoubleVar Double
            | Character Char
            | IfStatement
            | ElseStatement
            | WhileStatement
            | ForStatement
            | ReturnStatement
    deriving(Show, Eq)

-- AST of C for the Parser

data Program = Program Members
    deriving Show

data Members    = MemberBlock [Members]
                | MemberDeclaration Variable 
                | MemberStatement Statements
                | MemberFunction VarType Identifier [Variable] Statements
                | MemberEnum Enumerator
                | MemberStruct Struct
    deriving Show

data Enumerator = Enum String [Expression]
    deriving Show

data Struct = Struct String [Variable]
    deriving Show


data Statements     = StatementBlock [Statements] 
                    | StatementDeclaration Variable
                    | StatementExpression Expression
                    | StatementReturn Expression
                    | StatementIfElse Expression Statements Statements
                    | StatementWhile Expression Statements
    deriving Show

data Expression     = BinaryExp Operator Expression Expression
                    | UnaryExpression Expression Operator
                    | UnaryExpression2 Operator Expression
                    | FuncCall Identifier [Expression]
                    | LitInt Int 
                    | LitChar Char 
                    | LitDouble Double
                    | LitVar String
                    | LitPointer VarType String
                    | LitArray String Int
                    -- put() and printf() need to be hardcoded into the compiler, both print a string
                    -- scanf() asks for input
                    -- basic math functions also need to be hard coded in (think sin, cos , tan, etc)
    deriving Show

data VarType    = Void 
                | IntType 
                | CharType
                | DoubleType
                | PointerType VarType

                | SelfDefined String --Enums
                | StructType String --Structs
    deriving(Show, Eq)

data Variable   = Var Modifier VarType Identifier
                | ArrayVar Modifier VarType Identifier Int
                --the int is the length of the array
    deriving Show

data Modifier   = None 
                | Static 
                | Signed 
                | Unsigned
    deriving(Show, Eq)

data Operator   = Mul
                | Div
                | Mod
                | Add
                | Min

                | AndOp
                | OrOp
                | NotOp
                | XorOp

                | LessThan
                | LessOrEqualTo
                | GreaterThan
                | GreaterOrEqualTo
                | EqualTo
                | NotEqualTo

                | Assign
                | MulAssign
                | DivAssign
                | ModAssign
                | AddAssign
                | MinAssign
                | AndAssign
                | OrAssign
                | XorAssign

                | AddOne
                | MinusOne
                | NotOperator

                | AddressOperator -- &
                | PointerOperator -- *
    deriving(Show, Eq)
                
type Identifier = String;