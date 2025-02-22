module Lexer where

import AbstractSyntax

import Library.Library
import Library.ParserCombinators
import Prelude hiding ((<$>),(<*>),(<|>),(<$))
import Library.ElementaryParsers

lexSpace :: Parser Char Token
lexSpace = Space <$ parseSpaces

lexSingleLineComment :: Parser Char Token -- doesn't work properly yet
lexSingleLineComment = (\_ _ -> Comment) <$> token "//" <*> greedy (satisfy (/= '\n'))

lexMultiLineComment :: Parser Char Token -- doesn't work at all yet
lexMultiLineComment = (\_ _ _ -> Comment) <$> token "/*" <*> greedy parseAnySymbol <*> token "*/"

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
lexIdentifier = Name <$> greedy lower

lexStatementTokens :: Parser Char Token
lexStatementTokens  = (IfStatement <$ token "if") 
                    <|> (ElseStatement <$ token "else") 
                    <|> (WhileStatement <$ token "while")
                    <|> (ForStatement <$ token "for")
                    <|> (ReturnStatement <$ token "return")

lexIntVar :: Parser Char Token
lexIntVar = IntegerVar <$> parseInteger

lexDoubleVar :: Parser Char Token
lexDoubleVar = DoubleVar <$> parseDouble

lexCharVar :: Parser Char Token
lexCharVar = (\_ a _ -> Character a ) <$> symbol '\'' <*> parseChar <*> symbol '\''

lexModifier :: Parser Char Token
lexModifier =   ((\_ -> Modifier Static)<$>token "static") 
                <|> ((\_ -> Modifier Signed) <$> token "signed") 
                <|> ((\_ -> Modifier Unsigned)<$>token "unsigned")

lexType :: Parser Char Token
lexType =   ((\_ -> Type IntType) <$> token "int") 
            <|> ((\_ -> Type DoubleType) <$> token "double") 
            <|> ((\_ -> Type Void) <$> token "void")
            <|> (Type CharType <$ token "char")

operatorList :: [(Operator, String)]
operatorList =  [
                (MinAssign,"-="),(AddAssign,"+="),(DivAssign,"/="),(MulAssign,"*="),(ModAssign,"%="),
                (AndAssign,"&="),(OrAssign,"|="),(XorAssign,"^="),(EqualTo,"=="),(NotEqualTo,"!="),
                (AddOne,"++"),(MinusOne,"--"),
                (LessOrEqualTo,"<="),(GreaterOrEqualTo,">="),(LessThan,"<"),(GreaterThan,">"),
                (AndOp,"&&"),(OrOp,"||"),(NotOp,"!"),
                (Mul,"*"),(Add,"+"),(Min,"-"),(Div,"/"),(Mod,"%")
                ]

foldOverOperators :: [(Operator,String)] -> Parser Char Operator
foldOverOperators = foldr ((<<|>) . (\(a,b) -> a <$ token b)) (Assign <$ token "=")

lexOperator :: Parser Char Token
lexOperator = Operator <$> foldOverOperators operatorList

lexers :: [Parser Char Token]
lexers =    [
            lexSpace,
            lexSingleLineComment,
            lexMultiLineComment,
            lexSemicolon,
            lexClosingSquareBracket,
            lexOpeningSquareBracket,
            lexClosingRoundBracket,
            lexOpeningRoundBracket,
            lexOpeningBracket,
            lexClosingBracket,
            lexOperator,
            lexComma,
            lexDoubleVar,
            lexIntVar,
            lexCharVar,
            lexModifier,
            lexType,
            lexStatementTokens,
            lexIdentifier
            ]

filteringFunction :: Token -> Bool
filteringFunction Space = False
filteringFunction Comment = False
filteringFunction _ = True

lexer :: Parser Char [Token]
lexer = filter filteringFunction <$> greedy (greedyChoice lexers)