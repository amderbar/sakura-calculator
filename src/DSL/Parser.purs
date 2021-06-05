module Macro.DSL.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either)
import Data.Int (pow)
import Text.Parsing.Parser (Parser, ParseError, ParserT, runParser)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (char, oneOf, eof)
import Text.Parsing.Parser.Token (LanguageDef, TokenParser, GenLanguageDef(..), unGenLanguageDef, makeTokenParser, alphaNum, letter)

parseDSL :: String -> Either ParseError Int
parseDSL input = runParser input (expression <* eof)

sakuraCalcLangage :: LanguageDef
sakuraCalcLangage =
  LanguageDef
    (unGenLanguageDef emptyDef)
      { nestedComments = false
      , identStart = letter
      , identLetter = alphaNum <|> char '_'
      , opStart = op'
      , opLetter = op'
      , reservedOpNames = []
      , reservedNames = []
      , caseSensitive = false
      }
  where
  op' :: forall m. (Monad m) => ParserT String m Char
  op' = oneOf [ '*', '+', '/', '%', '-', '^' ]

tokenParser :: TokenParser
tokenParser = makeTokenParser sakuraCalcLangage

expression :: Parser String Int
expression = fix \expr -> buildExprParser operatorTable (integer <|> parens expr)
  where
  integer = tokenParser.integer
  parens = tokenParser.parens
  reservedOp = tokenParser.reservedOp
  operatorTable =
    [ [ Infix (reservedOp "^" $> pow) AssocLeft ]
    , [ Infix (reservedOp "/" $> (/)) AssocLeft ]
    , [ Infix (reservedOp "%" $> mod) AssocLeft ]
    , [ Infix (reservedOp "*" $> (*)) AssocLeft ]
    , [ Infix (reservedOp "-" $> (-)) AssocLeft ]
    , [ Infix (reservedOp "+" $> (+)) AssocLeft ]
    , [ Prefix (reservedOp "-" $> negate) ]
    ]
