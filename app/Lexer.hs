module Lexer where

import AbstractSyntax

import Library.Library
import Library.ParserCombinators
import Library.ElementaryParsers
import Control.Applicative
import Data.Char


lexSpace :: Parser Char Token
lexSpace = SpaceToken <$ greedy parseSpaces

lexSingleLineComment :: Parser Char Token
lexSingleLineComment = Comment <$ token "//" <* greedy (satisfy (/= '\n'))

lexSingleLineComment2 :: Parser Char Token
lexSingleLineComment2 = Comment <$ token "//"

lexMultiLineComment :: Parser Char Token
lexMultiLineComment =   (Comment <$ token "/*" <* greedy (nottoken "*/") <* token "*/")
                        <|> (Comment <$ token "/*" <* token "*/")

lexSemicolon :: Parser Char Token
lexSemicolon = Semicolon <$ symbol ';'

lexClosingSquareBracket :: Parser Char Token
lexClosingSquareBracket = ClosingSquareBracket <$ symbol ']'

lexOpeningSquareBracket :: Parser Char Token
lexOpeningSquareBracket = OpeningSquareBracket <$ symbol '['

lexClosingRoundBracket :: Parser Char Token
lexClosingRoundBracket = ClosingRoundBracket <$ symbol ')'

lexOpeningRoundBracket :: Parser Char Token
lexOpeningRoundBracket = OpeningRoundBracket <$ symbol '('

lexOpeningBracket :: Parser Char Token
lexOpeningBracket = OpeningBracket <$ symbol '{'

lexClosingBracket :: Parser Char Token
lexClosingBracket = ClosingBracket <$ symbol '}'

lexComma :: Parser Char Token
lexComma = Comma <$ symbol ','

lexIdentifier :: Parser Char Token
lexIdentifier = ((\a b -> Name (a:b)) <$> lower <*> greedy parseAnySymbol)
                <<|> (Name . (: []) <$> lower)

lexLibraryIdentifier :: Parser Char Token
lexLibraryIdentifier = (\_ a _ -> LibraryIdentifier a) <$> symbol '<' <*> greedy (satisfy ( /= '>')) <*> symbol '>'

lexStatementTokens :: Parser Char Token
lexStatementTokens  = (IfStatement <$ token "if") 
                    <|> (ElseStatement <$ token "else") 
                    <|> (WhileStatement <$ token "while")
                    <|> (ForStatement <$ token "for")
                    <|> (ReturnStatement <$ token "return")
                    <|> (IncludeStatement <$ token "#include")
                    <|> (EnumToken <$ token "enum")
                    <|> (StructToken <$ token "struct")

lexIntVar :: Parser Char Token
lexIntVar = IntegerVar <$> parseInteger

lexDoubleVar :: Parser Char Token
lexDoubleVar = DoubleVar <$> parseDouble

lexCharVar :: Parser Char Token
lexCharVar = Character <$> pack (symbol '\'') parseChar (symbol '\'')

lexModifier :: Parser Char Token
lexModifier =   (Modifier Static <$ token "static") 
                <|> (Modifier Signed <$ token "signed") 
                <|> (Modifier Unsigned <$ token "unsigned")

lexPointerType :: Parser Char Token
lexPointerType = (\(Type x) _ -> Type (PointerType x)) <$> lexType <*> symbol '*' 

lexType :: Parser Char Token
lexType =   (Type IntType <$ token "int") 
            <|> (Type DoubleType <$ token "double") 
            <|> (Type Void <$ token "void")
            <|> (Type CharType <$ token "char")

operatorList :: [(Operator, String)]
operatorList =  [
                (MinAssign,"-="),(AddAssign,"+="),(DivAssign,"/="),(MulAssign,"*="),(ModAssign,"%="),
                (AndAssign,"&="),(OrAssign,"|="),(XorAssign,"^="),(EqualTo,"=="),(NotEqualTo,"!="),
                (AddOne,"++"),(MinusOne,"--"), (AddressOperator,"&"),
                (LessOrEqualTo,"<="),(GreaterOrEqualTo,">="),(LessThan,"<"),(GreaterThan,">"),
                (AndOp,"&&"),(OrOp,"||"),(NotOp,"!"),
                (Mul,"*"),(Add,"+"),(Min,"-"),(Div,"/"),(Mod,"%")
                ]

foldOverOperators :: [(Operator,String)] -> Parser Char Operator
foldOverOperators = foldr ((<<|>) . (\(a,b) -> a <$ token b)) (Assign <$ token "=")

lexOperator :: Parser Char Token
lexOperator = Operator <$> foldOverOperators operatorList

lexPointerOperator :: Parser Char Token
lexPointerOperator = Parser $ \input -> case input of
    ('*':x:xs)  | isAlpha x -> [(Operator PointerOperator,x:xs)]
                | otherwise -> [(Operator Mul, x:xs)]
    (_:xs) -> []
    [] -> []

lexers :: [Parser Char Token]
lexers =    [
            lexSingleLineComment,
            lexSingleLineComment2,
            lexMultiLineComment,
            lexSemicolon,
            lexClosingSquareBracket,
            lexOpeningSquareBracket,
            lexClosingRoundBracket,
            lexOpeningRoundBracket,
            lexOpeningBracket,
            lexClosingBracket,
            lexPointerOperator,
            lexLibraryIdentifier,
            lexOperator,
            lexComma,
            lexDoubleVar,
            lexIntVar,
            lexCharVar,
            lexModifier,
            lexPointerType,
            lexType,
            lexStatementTokens,
            lexIdentifier
            ]

filteringFunction :: Token -> Bool
filteringFunction SpaceToken = False
filteringFunction Comment = False
filteringFunction _ = True

lexer :: Parser Char [Token]
lexer = fmap (filter filteringFunction) (greedy (lexSpace *> greedyChoice lexers))