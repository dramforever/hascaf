module Hascaf.Passes.Parser where

import           Control.Monad
import           Data.Char
import qualified Data.Text as T
import           Data.Void (Void)
import           Hascaf.Types.AST
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- * Top-level

parseTestProgram :: T.Text -> IO ()
parseTestProgram = parseTest program

parseProgram :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) Program
parseProgram = parse program

program :: Parser Program
program = Program <$ sc <*> manyTill topLevel eof

topLevel :: Parser TopLevel
topLevel = FunctionTop <$> function

function :: Parser Function
function = Function <$> typ <*> ident <* symbol "(" <* symbol ")" <*> compound
    <?> "function"

-- * Statements

compound :: Parser Compound
compound = symbol "{" *> manyTill stmt (symbol "}")
    <?> "compound_statement"

stmt :: Parser Stmt
stmt = ReturnS <$ reserved "return" <*> expr <* symbol ";"
    <?> "statement"

-- * Types

typ :: Parser Typ
typ = IntTyp <$ reserved "int"
    <?> "type"

-- * Expressions

expr :: Parser Expr
expr = IntLit <$> intLit
    <?> "expression"

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

nameWord :: Parser T.Text
nameWord = T.cons <$> satisfy isStart <*> takeWhileP Nothing isEnd
    where
        isStart c = isAsciiUpper c || isAsciiLower c || c == '_'
        isEnd c = isStart c || isDigit c

reservedWords :: [T.Text]
reservedWords =
    [ "int"
    , "return"
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
