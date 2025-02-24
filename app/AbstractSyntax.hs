module AbstractSyntax where

-- Tokens for the Lexer

data Token  = Space
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
            | BooleanValue Bool
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
                    | FuncCall Identifier [Expression]
                    | LitInt Int 
                    | LitChar Char 
                    | LitDouble Double
                    | LitVar String
                    | LitArray String Int
                    | LitBool Bool
                    -- put() and printf() need to be hardcoded into the compiler, both print a string
                    -- scanf() asks for input
                    -- basic math functions also need to be hard coded in (think sin, cos , tan, etc)
    deriving Show

data VarType    = Void 
                | IntType 
                | CharType
                | DoubleType
                | BoolType
                | PointerType
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
    deriving(Show, Eq)
                
type Identifier = String;