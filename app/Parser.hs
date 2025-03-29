module Parser where
import AbstractSyntax
import Library.Library
import Library.ElementaryParsers
import Library.ParserCombinators 
import Prelude hiding ((<$>),(<*>),(<|>),(<<|>),(<*),(*>),(<$))

parser :: Parser Token Program
parser = pProgram <* eof

pProgram :: Parser Token Program
pProgram = Program <$> pMemberBlock

pMember :: Parser Token Members
pMember =   pMemberStatement
            <|> pMemberDeclaration
            <|> pMemberFunction

pMemberBlock :: Parser Token Members
pMemberBlock = MemberBlock <$> greedy pMember

pMemberDeclaration :: Parser Token Members
pMemberDeclaration = MemberDeclaration <$> pVar <* pSemi

pMemberStatement :: Parser Token Members
pMemberStatement = MemberStatement <$> pStatementBlock

pMemberFunction :: Parser Token Members
pMemberFunction = MemberFunction 
    <$> pType
    <*> parseTokenName
    <*> pack (punc OpeningRoundBracket) (listOf pVar (punc Comma) <<|> succeed []) (punc ClosingRoundBracket)
    <*> pack (punc OpeningBracket) (pStatementBlock <<|> succeed (StatementBlock [])) (punc ClosingBracket)

pStatement :: Parser Token Statements
pStatement =    pStatementExpression
                <|> pStatementDeclaration
                <|> pStatementIfElse
                <|> pStatementWhile
                <|> pStatementFor
                <|> pStatementReturn

pStatementBlock :: Parser Token Statements
pStatementBlock = StatementBlock <$> greedy pStatement

pStatementDeclaration :: Parser Token Statements
pStatementDeclaration = (StatementDeclaration <$> pVar
                        <|> ((\a b c (Operator d) e-> StatementBlock [StatementDeclaration (Var a b c), StatementExpression (BinaryExp d e (LitVar c))]) 
                        <$> (pModifier <<|> succeed None)
                        <*> pType
                        <*> parseTokenName
                        <*> symbol (Operator Assign)
                        <*> e1)) <* pSemi

pStatementDeclarationAndExpression :: Parser Token Statements
pStatementDeclarationAndExpression = undefined

pStatementExpression :: Parser Token Statements
pStatementExpression = StatementExpression <$> e1 <* pSemi

pStatementReturn :: Parser Token Statements
pStatementReturn = StatementReturn <$> pack (punc ReturnStatement) e1 pSemi

pStatementIfElse :: Parser Token Statements
pStatementIfElse =  StatementIfElse
                    <$> punc IfStatement
                    *> bracketedExpressions
                    <*> bracketedStatements
                    <*> maybeElse
    where
        maybeElse = (punc ElseStatement *> bracketedStatements)
                    <|> succeed (StatementBlock [])


pStatementWhile :: Parser Token Statements
pStatementWhile = StatementWhile <$> punc WhileStatement *> bracketedExpressions <*> bracketedStatements

--desugering for statements to while loops
pStatementFor :: Parser Token Statements
pStatementFor = (\a b c d e -> StatementBlock [StatementDeclaration a, StatementWhile b (StatementBlock [e, StatementExpression d])])
    <$> punc ForStatement
    *> punc OpeningRoundBracket
    *> pVar
    <*> punc Comma
    *> e1
    <*> pSemi
    *> e1
    <*> pSemi
    *> e1
    <*> punc ClosingRoundBracket
    *>  pack (punc OpeningBracket) (pStatementBlock <<|> succeed (StatementBlock [])) (punc ClosingBracket)

pExpression :: Parser Token Expression
pExpression =   pLiteral
                <|> pFuncCall
                <|> bracketedExpressions
                <|> pUnaryExpression
                <|> pack (punc OpeningRoundBracket) pUnaryExpression (punc ClosingRoundBracket)

pUnaryExpression :: Parser Token Expression
pUnaryExpression =  (UnaryExpression <$> pLiteral <*> pOperator)
                    <|> (UnaryExpression2 <$> pOperator <*> pLiteral)

genr :: [Operator] -> Parser Token Expression -> Parser Token Expression
genr ops p = chainr p (choice (map f ops))
    where
        f s = (\(Operator x) -> BinaryExp x) <$> symbol (Operator s)

genl :: [Operator] -> Parser Token Expression -> Parser Token Expression
genl ops p = chainl p (choice (map f ops))
    where 
        f s = (\(Operator x) -> BinaryExp x) <$> symbol (Operator s)

-- [[Assign], [XorAssign, OrAssign], [AndAssign], [MinAssign, AddAssign], [ModAssign, DivAssign, MulAssign]]
rightAssociative :: [[Operator]]
rightAssociative = [[Assign], [MinAssign, AddAssign], [ModAssign, DivAssign, MulAssign]]
e1 :: Parser Token Expression
e1 = foldr genr e2 rightAssociative

leftAssociative :: [[Operator]]
leftAssociative = [[OrOp, XorOp],[AndOp],[NotEqualTo,EqualTo],[GreaterOrEqualTo,LessOrEqualTo],[GreaterThan,LessThan],[Min,Add],[Mod,Div,Mul]]
e2 :: Parser Token Expression
e2 = foldr genl pExpression leftAssociative

pLiteral :: Parser Token Expression
pLiteral =  pLitInt 
            <|> pLitVar
            <|> pLitDouble 
            <|> pLitChar 
            <|> pLitArray

pLitInt :: Parser Token Expression
pLitInt = LitInt <$> parseTokenInt

pLitDouble :: Parser Token Expression
pLitDouble = LitDouble <$> parseTokenDouble

pLitChar :: Parser Token Expression
pLitChar = LitChar <$> parseTokenChar

pLitVar :: Parser Token Expression
pLitVar = LitVar <$> parseTokenName

pLitArray :: Parser Token Expression
pLitArray = LitArray <$> parseTokenName <*> pack (punc OpeningSquareBracket) parseTokenInt (punc ClosingSquareBracket)

--The reason the pFuncCall function is so slow is the listOf e1 (punc Comma)
--after testing, I figured out it slows it down majorly
pFuncCall :: Parser Token Expression
pFuncCall = FuncCall <$> parseTokenName <*> pack (punc OpeningRoundBracket) (listOf e1 (punc Comma) <|> succeed []) (punc ClosingRoundBracket)

pVar :: Parser Token Variable
pVar = pNormalVar <|> pArrayVar

pNormalVar :: Parser Token Variable
pNormalVar = Var
    <$> (pModifier <<|> succeed None)
    <*> pType
    <*> parseTokenName

-- I need to change the way arrays are parsed, it is not done correctly
pArrayVar :: Parser Token Variable
pArrayVar = ArrayVar
    <$> (pModifier <<|> succeed None)
    <*> pType
    <*> parseTokenName
    <*> pack (punc OpeningSquareBracket) parseTokenInt (punc ClosingSquareBracket)

punc :: Token -> Parser Token Token
punc = symbol 

bracketedExpressions :: Parser Token Expression
bracketedExpressions = pack (punc OpeningRoundBracket) e1 (punc ClosingRoundBracket)
bracketedStatements :: Parser Token Statements
bracketedStatements = pack (punc OpeningBracket) pStatementBlock (punc ClosingBracket)

pSemi :: Parser Token Token
pSemi = punc Semicolon

parseTokenInt :: Parser Token Int
parseTokenInt ((IntegerVar x):xs) = [(x,xs)]
parseTokenInt _ = failp []

parseTokenDouble :: Parser Token Double
parseTokenDouble ((DoubleVar x):xs) = [(x,xs)]
parseTokenDouble _ = failp []

parseTokenChar :: Parser Token Char
parseTokenChar ((Character x):xs) = [(x,xs)]
parseTokenChar _ = failp []

parseTokenName :: Parser Token Identifier
parseTokenName ((Name x):xs) = [(x,xs)]
parseTokenName _ = failp []

pType :: Parser Token VarType
pType ((Type x):xs) = [(x,xs)]
pType _ = failp []

pModifier :: Parser Token Modifier
pModifier ((Modifier x):xs) = [(x,xs)]
pModifier _ = failp []

pOperator :: Parser Token Operator
pOperator ((Operator x):xs) = [(x,xs)]
pOperator _ = failp []
