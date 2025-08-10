{-# LANGUAGE InstanceSigs #-}
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
            | IncludeStatement
            | String String
            | EnumToken
            | StructToken
            | TypedefToken
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
                | MemberInclude String
                | MemberTypedef TypeDef
    deriving Show

data Enumerator = Enum Identifier Statements
                | EnumTypedef Identifier
    deriving Show

data Struct = Struct Identifier [Variable]
                | StructTypedef Identifier
    deriving Show

data TypeDef =  Def1 Struct Identifier
                | Def2 Modifier VarType Identifier
                | Def3 Enumerator Identifier
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
                    | LitVar Identifier
                    | LitPointer VarType Identifier
                    | LitArray Identifier Int
                    -- put() and printf() need to be hardcoded into the compiler, both print a string
                    -- scanf() asks for input
                    -- basic math functions also need to be hard coded in (think sin, cos , tan, etc)
    deriving Show

data VarType    = Void 
                | IntType 
                | CharType
                | DoubleType
                | PointerType VarType

                | SelfDefined Identifier
    deriving(Show)

instance Eq VarType where
    (==) :: VarType -> VarType -> Bool
    (==) Void Void = True
    (==) DoubleType DoubleType = True
    (==) IntType IntType = True
    (==) CharType CharType = True
    (==) (PointerType x) (PointerType y) = x == y
    (==) (SelfDefined x) (SelfDefined y) = x == y
    (==) _ _ = False
    (/=) :: VarType -> VarType -> Bool
    (/=) a b = not (a == b)

data Variable   = Var Modifier VarType Identifier
                | ArrayVar Modifier VarType Identifier Int
                | EnumVar VarType Identifier
                | StructVar VarType Identifier
    deriving Show

data Modifier   = None 
                | Static 
                | Signed 
                | Unsigned
    deriving(Show, Eq)

-- https://en.cppreference.com/w/c/language/operator_precedence
data Operator   = AddOne
                | MinOne
                | UnaryPlus
                | UnaryMinus
                | LogicalNot
                | BitwiseNot
                | Indirection -- pointer *
                | AddressOf -- variablee &
                | Mul
                | Div
                | Mod
                | Add
                | Min
                | BitwiseLeft
                | BitwiseRight
                | LessThan
                | LessOrEqual
                | GreaterThan
                | GreaterOrEqual
                | EqualComp
                | NotEqualComp
                | BitwiseAnd
                | BitwiseXor
                | BitwiseOr
                | LogicalAnd
                | LogicalOr
                | Assignment
                | AddAss
                | MinAss
                | MulAss
                | DivAss
                | ModAss
                | BitwiseLeftAss
                | BitwiseRightAss
                | BitwiseAndAss
                | BitwiseXorAss
                | BitwiseOrAss
                | NegativeOperator

                | StructElement
    deriving(Show, Eq)
                
type Identifier = String;