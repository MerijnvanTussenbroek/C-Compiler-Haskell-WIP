module Lexer where

import AbstractSyntax

import Library.Library
import Library.ParserCombinators
import Library.ElementaryParsers
import Control.Applicative
import Data.Char

lexSpace :: Parser Char Token
lexSpace = SpaceToken <$ greedy parseSpaces

lexComment :: Parser Char Token
lexComment =    (Comment <$ token "//" <* greedy (satisfy (/= '\n')))
                <|>(Comment <$ token "/*" <* greedy (nottoken "*/") <* token "*/")

lexKeyWords :: Parser Char Token
lexKeyWords =   IfStatement <$ token "if"
                <|> ElseStatement <$ token "else"
                <|> WhileStatement <$ token "while"
                <|> ForStatement <$ token "for"
                <|> ReturnStatement <$ token "return"
                <|> IncludeStatement <$ token "#include"
                <|> EnumToken <$ token "enum"
                <|> StructToken <$ token "struct"
                <|> TypedefToken <$ token "typedef"

lexTypes :: Parser Char Token
lexTypes =  Type Void <$ token "void"
            <|> Type IntType <$ token "int"
            <|> Type CharType <$ token "char"
            <|> Type DoubleType <$ token "double"

lexModifier :: Parser Char Token
lexModifier =   Modifier Static <$ token "static"
                <|> Modifier Signed <$ token "signed"
                <|> Modifier Unsigned <$ token "unsigned"

lexLibraryIdentifier :: Parser Char Token
lexLibraryIdentifier =  Name <$ symbol '<' <*> greedy (satisfy (/= '>')) <* symbol '>'

lexIdentifier :: Parser Char Token
lexIdentifier =     (\a b -> Name (a:b)) 
                    <$> satisfy firstLetterParserHelperFunc
                    <*> greedy parseAnySymbol

operators :: [(Operator,String)]
operators = [
            (AddOne,"++"),(MinOne,"--"),
            (EqualComp,"=="),(NotEqualComp,"!="),
            (BitwiseOrAss,"|="),(BitwiseXorAss,"^="),(BitwiseAndAss,"&="),
            (BitwiseLeftAss,"<<="),(BitwiseRightAss,">>="),
            (ModAss,"%="),(DivAss,"/="),(MulAss,"*="),
            (AddAss,"+="),(MinAss,"-="),
            (LogicalOr,"||"),(LogicalAnd,"&&"),
            (GreaterOrEqual,">="),(LessOrEqual,"<="),
            (BitwiseLeft,"<<"),(BitwiseRight,">>"),
            (Add,"+"),(Min,"-"),(Mul,"*"),(AddressOf,"&"),(Div,"/"),(Mod,"%"),
            (LessThan,"<"),(GreaterThan,">"),(LogicalNot,"!"),(BitwiseNot,"~"),
            (BitwiseAnd,"&"),(BitwiseOr,"|"),(BitwiseXor,"^")
            ]

lexOperator :: Parser Char Token
lexOperator = foldr (\(a,b) -> (Operator a <$ token b <<|>)) (Operator Assignment <$ token "=") operators

lexBrackets :: Parser Char Token
lexBrackets =   OpeningBracket <$ symbol '{'
                <|> ClosingBracket <$ symbol '}'
                <|> OpeningSquareBracket <$ symbol '['
                <|> ClosingSquareBracket <$ symbol ']'
                <|> OpeningRoundBracket <$ symbol '('
                <|> ClosingRoundBracket <$ symbol ')'

lexSymbols :: Parser Char Token
lexSymbols =    Semicolon <$ symbol ';'
                <|> Comma <$ symbol ','
                <|> Apostrofy <$ symbol '\''

lexInteger :: Parser Char Token
lexInteger = IntegerVar <$> parseInteger

lexDouble :: Parser Char Token
lexDouble = DoubleVar <$> parseDouble

lexChar :: Parser Char Token
lexChar = Character <$> pack (symbol '\'') (satisfy (/= '\'')) (symbol '\'')

lexString :: Parser Char Token
lexString = String <$> pack (symbol '\"') (greedy (satisfy (/= '\"'))) (symbol '\"')

lexers :: [Parser Char Token]
lexers =    [
            lexComment,
            lexKeyWords,
            lexTypes,
            lexModifier,
            lexBrackets,
            lexSymbols,
            lexInteger,
            lexDouble,
            lexChar,
            lexString,
            lexOperator,
            lexLibraryIdentifier,
            lexIdentifier
            ]



filteringFunction :: Token -> Bool
filteringFunction SpaceToken = False
filteringFunction Comment = False
filteringFunction _ = True

lexer :: Parser Char [Token]
lexer = fmap (filter filteringFunction) (greedy (lexSpace *> greedyChoice lexers))