module Hascaf.Passes.Parser where

import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Foldable (toList)
import           Data.List (sortOn)
import qualified Data.Map as M
import           Data.Ord (Down(..))
import qualified Data.Text as T
import           Data.Void (Void)
import           Hascaf.Types.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- * Top-level

parseProgram :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) (Program Syn)
parseProgram = parse program

program :: Parser (Program Syn)
program = Program <$ sc <*> manyTill topLevel eof

topLevel :: Parser (TopLevel Syn)
topLevel = FunctionTop <$> function

function :: Parser (Function Syn)
function = Function () <$> typ <*> ident <* symbol "(" <* symbol ")" <*> compound
    <?> "function"

-- * Statements

compound :: Parser (Compound Syn)
compound = Compound <$ symbol "{" <*> manyTill blockItem (symbol "}")
    <?> "compound_statement"

blockItem :: Parser (Stmt Syn)
blockItem =
    stmt
    <|> DeclS <$> typ <*> var <*> optional (symbol "=" *> expr) <* symbol ";"

stmt :: Parser (Stmt Syn)
stmt =
    ReturnS <$ reserved "return" <*> expr <* symbol ";"
    <|> EmptyS <$ symbol ";"
    <|> CompoundS <$> compound
    <|> IfS ()
        <$ reserved "if"
        <*> between (symbol "(") (symbol ")") expr
        <*> stmt
        <*> optional (reserved "else" *> stmt)
    <|> ExprS <$> expr <* symbol ";"
    <?> "statement"

-- * Types

typ :: Parser Typ
typ = IntTyp <$ reserved "int"
    <?> "type"

-- * Expressions

allUnaryOps :: M.Map UnaryOp T.Text
allUnaryOps = M.fromList
    [ (Neg, "-")
    , (Not, "~")
    , (LNot, "!")
    ]

allBinaryOps :: M.Map BinaryOp T.Text
allBinaryOps = M.fromList
    [ (Add, "+")
    , (Sub, "-")
    , (Mul, "*")
    , (Div, "/")
    , (Mod, "%")
    , (Eq, "==")
    , (Ne, "!=")
    , (Le, "<=")
    , (Ge, ">=")
    , (Lt, "<")
    , (Gt, ">")
    , (LAnd, "&&")
    , (LOr, "||")
    ]

expr :: Parser (Expr Syn)
expr = foldr1 Assignment <$> sepBy1 condExpr (symbol "=")

condExpr :: Parser (Expr Syn)
condExpr = do
    e <- opExpr
    (Ternary () e <$ symbol "?" <*> expr <* symbol ":" <*> condExpr)
        <|> pure e

opExpr :: Parser (Expr Syn)
opExpr = makeExprParser primary $ [[ unary ]] ++ binary
    where
        unaryTable = M.keys allUnaryOps
        unary = Prefix $ foldr1 (.) <$> some (msum (unaryOp <$> unaryTable))

        binaryTable =
            [ [Mul, Div, Mod]
            , [Add, Sub]
            , [Le, Ge, Lt, Gt]
            , [Eq, Ne]
            , [LAnd]
            , [LOr]
            ]

        binary = (fmap . fmap) (InfixL . binaryOp) binaryTable

-- Longest match tokenization for operators

allOpNames :: [T.Text]
allOpNames = sortOn (Down . T.length) $
    toList allUnaryOps ++ toList allBinaryOps ++ [ "=" ]

parseOpName :: T.Text -> Parser ()
parseOpName opn = try (msum (symbol <$> allOpNames) >>= guard . (== opn))
    <?> ("'" ++ T.unpack opn ++ "'")

unaryOp :: UnaryOp -> Parser (Expr x -> Expr x)
unaryOp op = Unary op <$ parseOpName (allUnaryOps M.! op)

binaryOp :: BinaryOp -> Parser (Expr x -> Expr x -> Expr x)
binaryOp op = Binary op <$ parseOpName (allBinaryOps M.! op)

-- ** Primary expression

primary :: Parser (Expr Syn)
primary =
    IntLit <$> intLit
    <|> VarRef <$> var
    <|> between (symbol "(") (symbol ")") expr
    <?> "expression"

var :: Parser (Var Syn)
var = Var () <$> ident

intLit :: Parser Integer
intLit = do
    val <- lexeme . try $ L.decimal
    when (val < 0 || val >= 0x8000_0000) $
        fail ("Integer literal " ++ show val ++ " out of range")
    pure val

-- * Utilities

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = try . L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = try . L.symbol sc

atom :: String -> Parser a -> Parser a
atom lbl = label lbl . lexeme

-- Longest match tokenization for names

nameWord :: Parser T.Text
nameWord = T.cons <$> satisfy isStart <*> takeWhileP Nothing isEnd
    where
        isStart c = isAsciiUpper c || isAsciiLower c || c == '_'
        isEnd c = isStart c || isDigit c

reservedWords :: [T.Text]
reservedWords =
    [ "int"
    , "return"
    , "if"
    , "else"
    ]

reserved :: T.Text -> Parser ()
reserved res | res `notElem` reservedWords =
    error $ "'" ++ T.unpack res ++ "' is not listed as reserved"
reserved res = atom ("'" ++ T.unpack res ++ "'") $
    nameWord >>= guard . (== res)

ident :: Parser Ident
ident = atom "identifier" $ do
    nw <- nameWord
    when (nw `elem` reservedWords) $
        fail ("'" ++ T.unpack nw ++ "' is a reserved word")
    when ("__" `T.isPrefixOf` nw) $
        fail ("Identifiers cannot start with two underscores")
    pure (Ident nw)
