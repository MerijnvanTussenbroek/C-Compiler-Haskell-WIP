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
            <|> pMemberFunction
            <|> pMemberEnum
            <|> pMemberStruct
            <|> pMemberInclude
            <|> pMemberTypedef

pMemberDeclaration :: Parser Token Members
pMemberDeclaration = (\(mod, vartype, exps) -> foldOverExpression mod vartype exps []) <$> pDeclaration <* pSemi

pMemberFunction :: Parser Token Members
pMemberFunction = do
    funcType <- parseType <|> (SelfDefined <$> parseTokenName)
    funcName <- parseTokenName
    p OpeningRoundBracket
    decls <- listOf pFuncDeclaration (p Comma) <<|> succeed []
    p ClosingRoundBracket
    p OpeningBracket
    body <- pStatementBlock
    p ClosingBracket
    return (MemberFunction funcType funcName decls body)

pFuncDeclaration :: Parser Token Variable
pFuncDeclaration = do
    mod <- litModifier
    vartype <- (parseType <|> (SelfDefined <$> parseTokenName))
    name <- parseTokenName
    return (Var mod vartype name)

pMemberEnum :: Parser Token Members
pMemberEnum = do
    p EnumToken
    enumName <- parseTokenName
    p OpeningBracket
    enumDecls <- pEnumDecls
    p ClosingBracket
    pSemi
    return (MemberEnum (Enum enumName enumDecls))
    

pEnumDecls :: Parser Token Statements
pEnumDecls = StatementBlock . reverse <$> listOf (StatementExpression <$> pBinaryExpression pLiteral) (p Comma)

pMemberStruct :: Parser Token Members
pMemberStruct = do
    p StructToken
    structName <- parseTokenName
    p OpeningBracket
    exps <- greedy (pStructDecls <* pSemi)
    p ClosingBracket
    pSemi
    return (MemberStruct (Struct structName exps))

pStructDecls :: Parser Token Variable
pStructDecls = do
    mod <- litModifier
    vartype <- parseType <|> (SelfDefined <$> parseTokenName)
    name <- parseTokenName
    return (Var mod vartype name)

pMemberInclude :: Parser Token Members
pMemberInclude = do
    p IncludeStatement
    MemberInclude <$> (parseTokenName <|> pString)

pMemberTypedef :: Parser Token Members
pMemberTypedef = 
    ((\_ a b c -> MemberTypedef (Def2 a b c)) <$> p TypedefToken <*> litModifier <*> parseType <*> parseTokenName <* pSemi) 
    <|> pMemberTypedefStruct
    <|> pMemberTypedefEnum

pMemberTypedefStruct :: Parser Token Members
pMemberTypedefStruct = do
    p TypedefToken
    p StructToken
    p OpeningBracket
    exps <- greedy (pStructDecls <* pSemi)
    p ClosingBracket
    structName <- parseTokenName
    pSemi
    return (MemberTypedef (Def1 (Struct structName exps) structName))

pMemberTypedefEnum :: Parser Token Members
pMemberTypedefEnum = do
    p TypedefToken
    p EnumToken
    p OpeningBracket
    exps <- pEnumDecls
    p ClosingBracket
    enumName <- parseTokenName
    pSemi
    return (MemberTypedef(Def3 (Enum enumName exps) enumName))
    

pStatementBlock :: Parser Token Statements
pStatementBlock = StatementBlock <$> greedy pStatement

pStatement :: Parser Token Statements
pStatement =    pStatementDeclaration
                <|> pStatementExpression
                <|> pStatementReturn
                <|> pStatementExpression
                <|> pStatementIfElse
                <|> pStatementWhile
                <|> pStatementFor

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

pStatementDeclaration :: Parser Token Statements
pStatementDeclaration = (\(mod, vartype, exps) -> foldOverExpression2 mod vartype exps []) <$> pDeclaration <* pSemi

pStatementWhile :: Parser Token Statements
pStatementWhile = do
    p WhileStatement
    p OpeningRoundBracket
    exp <- pExpression
    p ClosingRoundBracket
    p OpeningBracket
    sta <- pStatementBlock
    p ClosingBracket
    return (StatementWhile exp sta)

pStatementFor :: Parser Token Statements
pStatementFor = do
    p ForStatement
    p OpeningRoundBracket
    decls <- listOf pDeclaration (p Comma)
    pSemi
    exp1 <- pExpression
    pSemi
    exp2 <- listOf pExpression (p Comma)
    p ClosingRoundBracket
    p OpeningBracket
    body <- pStatementBlock
    p ClosingBracket
    return (StatementBlock [StatementBlock(forFolder2 decls), StatementWhile exp1 (StatementBlock [body,StatementBlock (forFolder1 exp2)])])

forFolder1 :: [Expression] -> [Statements]
forFolder1 = foldr ((:) . StatementExpression) []

forFolder2 :: [(Modifier, VarType, [Expression])] -> [Statements]
forFolder2 = foldr ((:) . (\(mod, vartype, exp) -> foldOverExpression2 mod vartype exp [])) []

pDeclaration :: Parser Token (Modifier, VarType, [Expression])
pDeclaration = do
    mod <- litModifier
    vartype <-  parseType <|> (SelfDefined <$> parseTokenName)
    exps <- listOf (pBinaryExpression pAfterBinaryExpression) (p Comma)
    return (mod, vartype, exps)

foldOverExpression :: Modifier -> VarType -> [Expression] -> [Members] -> Members
foldOverExpression mod var (alg@(BinaryExp Assignment (LitVar x) e2):xs) s = foldOverExpression mod var xs (MemberBlock[MemberDeclaration (Var mod var x), MemberStatement (StatementExpression alg)]:s)
foldOverExpression mod var (alg@(LitVar x):xs) s = foldOverExpression mod var xs (MemberDeclaration (Var mod var x):s)
foldOverExpression mod var (alg@(BinaryExp Assignment (UnaryExpression (LitVar x) Mul) e1):xs) s = undefined
foldOverExpression mod var [] s = MemberBlock (reverse s)

foldOverExpression2 :: Modifier -> VarType -> [Expression] -> [Statements] -> Statements
foldOverExpression2 mod var (alg@(BinaryExp Assignment (LitVar x) e2):xs) s = foldOverExpression2 mod var xs (StatementBlock [StatementDeclaration (Var mod var x), StatementExpression alg]:s)
foldOverExpression2 mod var (alg@(LitVar x):xs) s = foldOverExpression2 mod var xs (StatementDeclaration (Var mod var x):s)
foldOverExpression2 mod var [] s = StatementBlock (reverse s)

pExpression :: Parser Token Expression
pExpression =   (pBinaryExpression pAfterBinaryExpression
                <|> pLiteral
                <|> pFunctionCall)
                <|> rightUnary unaryLeftOperators pAfterBinaryExpression
                <|> leftUnary unaryRightOperators pAfterBinaryExpression

pAfterBinaryExpression :: Parser Token Expression
pAfterBinaryExpression =    (pLiteral
                            <|> pFunctionCall)
                            <|> rightUnary unaryLeftOperators pAfterUnaryExpression
                            <|> leftUnary unaryRightOperators pAfterUnaryExpression
                            <|> pack (p OpeningRoundBracket) pExpression (p ClosingRoundBracket)

pAfterUnaryExpression :: Parser Token Expression
pAfterUnaryExpression = (pLiteral
                        <|> pFunctionCall)
                        <|> pack (p OpeningRoundBracket) pExpression (p ClosingRoundBracket)

pFunctionCall :: Parser Token Expression
pFunctionCall = do
    name <- parseTokenName
    p OpeningRoundBracket
    exps <- listOf pExpression (p Comma) <<|> succeed []
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
                    , BitwiseAndAss, BitwiseXorAss, BitwiseOrAss,
                    StructElement
                    ]

leftGen :: [Operator] -> Parser Token Expression -> Parser Token Expression
leftGen ops p = chainr p (choice (map(\op -> (\(Operator op) -> BinaryExp op) <$> symbol (Operator op)) ops)) 

rightGen :: [Operator] -> Parser Token Expression -> Parser Token Expression
rightGen ops p = chainl p (choice (map(\op -> (\(Operator op) -> BinaryExp op) <$> symbol (Operator op)) ops))

leftUnary :: [Operator] -> Parser Token Expression -> Parser Token Expression
leftUnary ops p = choice (map(\op -> (\(Operator op) exp -> UnaryExpression exp op) <$> symbol (Operator op) <*> p) ops)

rightUnary :: [Operator] -> Parser Token Expression -> Parser Token Expression
rightUnary ops p = choice (map(\op -> (\exp (Operator op) -> UnaryExpression2 op exp) <$> p <*> symbol (Operator op)) ops)

pBinaryExpression :: Parser Token Expression -> Parser Token Expression
pBinaryExpression pAfter = leftGen leftAssociative (foldr rightGen pAfter rightAssociative)

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

litModifier :: Parser Token Modifier
litModifier = pModifier <<|> succeed None

parseType :: Parser Token VarType
parseType = 
    pType
    <|> (definePointer <$> pType <*> greedy (p (Operator Mul)))

definePointer :: VarType -> [Token] -> VarType
definePointer vartype (x:xs) = PointerType (definePointer vartype xs)
definePointer vartype [] = vartype

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

pString :: Parser Token String
pString = anySymbol >>= \case
    String x -> return x
    _ -> pure failp []