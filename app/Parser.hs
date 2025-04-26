{-# LANGUAGE LambdaCase #-}

module Parser where
import AbstractSyntax
import Library.Library
import Library.ElementaryParsers
import Library.ParserCombinators 
import Control.Applicative

p :: Token -> Parser Token Token
p = symbol

pSemi :: Parser Token Token
pSemi = p Semicolon

parser :: Parser Token Program
parser = pProgram <* eof

pProgram :: Parser Token Program
pProgram = Program <$> pMemberBlock

pMemberBlock :: Parser Token Members
pMemberBlock = MemberBlock <$> greedy pMember

pMember :: Parser Token Members
pMember =   pMemberDeclaration
            <|> pMemberStatement
            <|> pMemberFunction
            <|> pMemberEnum
            <|> pMemberStruct
            <|> pMemberInclude
            <|> pMemberTypedef

pMemberDeclaration :: Parser Token Members
pMemberDeclaration = undefined

pMemberStatement :: Parser Token Members
pMemberStatement = MemberStatement <$> pStatementBlock

pMemberFunction :: Parser Token Members
pMemberFunction = do
    funcType <- pType
    funcName <- parseTokenName
    p OpeningRoundBracket
    decls <- listOf pDeclaration (p Comma)
    p ClosingRoundBracket
    p OpeningBracket
    body <- pStatementBlock
    p ClosingBracket
    return (MemberFunction funcType funcName decls body)

pMemberEnum :: Parser Token Members
pMemberEnum = do
    p EnumToken
    enumName <- parseTokenName
    p OpeningBracket
    enumDecls <- pEnumDecls
    p ClosingBracket
    return (MemberEnum (Enum enumName enumDecls))
    

pEnumDecls :: Parser Token [(Variable, Expression)]
pEnumDecls = undefined

pMemberStruct :: Parser Token Members
pMemberStruct = undefined

pMemberInclude :: Parser Token Members
pMemberInclude = do
    p IncludeStatement
    name <- parseTokenName
    return (MemberInclude name)

pMemberTypedef :: Parser Token Members
pMemberTypedef = undefined

pStatementBlock :: Parser Token Statements
pStatementBlock = StatementBlock <$> greedy pStatement

pStatement :: Parser Token Statements
pStatement =    pStatementDeclaration
                <|> pStatementExpression
                <|> pStatementReturn
                <|> pStatementExpression
                <|> pStatementIfElse
                <|> pStatementWhile

pStatementDeclaration :: Parser Token Statements
pStatementDeclaration = undefined

pStatementExpression :: Parser Token Statements
pStatementExpression = do
    exp <- pExpression
    pSemi
    return (StatementExpression exp)

pStatementReturn :: Parser Token Statements
pStatementReturn = do
    p ReturnStatement
    exp <- pExpression
    pSemi
    return (StatementReturn exp)

pStatementIfElse :: Parser Token Statements
pStatementIfElse = do
    p IfStatement
    p OpeningRoundBracket
    exp <- pExpression
    p ClosingRoundBracket
    p OpeningBracket
    sta <- pStatementBlock
    p ClosingBracket
    elseSta <- elsePos <|> succeed (StatementBlock [])
    return (StatementIfElse exp sta elseSta)
    where
        elsePos = do
            p ElseStatement
            p OpeningBracket
            sta <- pStatementBlock
            p ClosingBracket
            return sta

pStatementWhile :: Parser Token Statements
pStatementWhile = do
    p WhileStatement
    p OpeningRoundBracket
    exp <- pExpression
    p ClosingBracket
    p OpeningBracket
    sta <- pStatementBlock
    p ClosingBracket
    return (StatementWhile exp sta)

pDeclaration :: Parser Token Variable
pDeclaration = undefined

pExpression :: Parser Token Expression
pExpression =   (pBinaryExpression
                <|> pLiteral
                <|> pFunctionCall)
                <<|> leftUnary unaryLeftOperators pAfterBinaryExpression
                <<|> rightUnary unaryRightOperators pAfterBinaryExpression

pAfterBinaryExpression :: Parser Token Expression
pAfterBinaryExpression =    (pLiteral
                            <|> pFunctionCall)
                            <<|> leftUnary unaryLeftOperators pAfterUnaryExpression
                            <<|> rightUnary unaryRightOperators pAfterUnaryExpression

pAfterUnaryExpression :: Parser Token Expression
pAfterUnaryExpression = pLiteral
                        <|> pFunctionCall

pFunctionCall :: Parser Token Expression
pFunctionCall = do
    name <- parseTokenName
    p OpeningRoundBracket
    exps <- listOf pExpression (p Comma)
    p ClosingRoundBracket
    return (FuncCall name exps)

-- left associative = right to left
-- right associative = left to right
-- right to left means first parsing the right then left
-- left to right means first parsing the left then the right

unaryRightOperators :: [Operator]
unaryRightOperators = [AddOne, MinOne, LogicalNot, BitwiseNot, Mul, AddressOf, Min]

unaryLeftOperators :: [Operator]
unaryLeftOperators = [AddOne, MinOne]

rightAssociative :: [[Operator]]
rightAssociative =  [
                    [LogicalOr],[LogicalAnd],
                    [BitwiseOr],[BitwiseXor],[BitwiseAnd],
                    [EqualComp, NotEqualComp],
                    [GreaterThan, GreaterOrEqual, LessThan,LessOrEqual],
                    [BitwiseLeft,BitwiseRight],
                    [Add,Min],
                    [Mul, Div, Mod]
                    ]

leftAssociative :: [Operator]
leftAssociative =   [Assignment
                    , AddAss, MinAss
                    , MulAss, DivAss, ModAss
                    , BitwiseLeftAss, BitwiseRightAss
                    , BitwiseAndAss, BitwiseXorAss, BitwiseOrAss
                    ]

leftGen :: [Operator] -> Parser Token Expression -> Parser Token Expression
leftGen ops p = chainr p (choice (map(\op -> (\(Operator op) -> BinaryExp op) <$> symbol (Operator op)) ops)) 

rightGen :: [Operator] -> Parser Token Expression -> Parser Token Expression
rightGen ops p = chainl p (choice (map(\op -> (\(Operator op) -> BinaryExp op) <$> symbol (Operator op)) ops))

leftUnary :: [Operator] -> Parser Token Expression -> Parser Token Expression
leftUnary ops p = choice (map(\op -> (\(Operator op) exp -> UnaryExpression exp op) <$> symbol (Operator op) <*> p) ops)

rightUnary :: [Operator] -> Parser Token Expression -> Parser Token Expression
rightUnary ops p = choice (map(\op -> (\exp (Operator op) -> UnaryExpression2 op exp) <$> p <*> symbol (Operator op)) ops)

pBinaryExpression :: Parser Token Expression
pBinaryExpression = leftGen leftAssociative (foldr rightGen pAfterBinaryExpression rightAssociative)

pLiteral :: Parser Token Expression
pLiteral = litInt <|> litChar <|> litDouble <|> litVar

litInt :: Parser Token Expression
litInt = LitInt <$> parseTokenInt

litChar :: Parser Token Expression
litChar = LitChar <$> parseTokenChar

litDouble :: Parser Token Expression
litDouble = LitDouble <$> parseTokenDouble

litVar :: Parser Token Expression
litVar = LitVar <$> parseTokenName

parseTokenInt :: Parser Token Int
parseTokenInt = anySymbol >>= \case
    IntegerVar x -> return x
    _ -> pure failp []

parseTokenDouble :: Parser Token Double
parseTokenDouble = anySymbol >>= \case
    DoubleVar x -> return x
    _ -> pure failp []

parseTokenChar :: Parser Token Char
parseTokenChar = anySymbol >>= \case
    Character x -> return x
    _ -> pure failp []

parseTokenName :: Parser Token Identifier
parseTokenName = anySymbol >>= \case
    Name x -> return x
    _ -> pure failp []

pType :: Parser Token VarType
pType = anySymbol >>= \case
    Type x -> return x
    _ -> pure failp []

pModifier :: Parser Token Modifier
pModifier = anySymbol >>= \case
    Modifier x -> return x
    _ -> pure failp []

pOperator :: Parser Token Operator
pOperator = anySymbol >>= \case
    Operator x -> return x
    _ -> pure failp []