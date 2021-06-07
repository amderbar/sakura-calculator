module Macro.DSL.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Macro.DSL.Core as DSL
import Text.Parsing.Parser (Parser, ParseError, runParser)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.String (eof)
import Text.Parsing.Parser.Token (LanguageDef, TokenParser, GenLanguageDef(..), unGenLanguageDef, makeTokenParser)

parseDSL :: String -> Either ParseError DSL.Expression
parseDSL input = runParser input (expression <* eof)

sakuraCalcLangage :: LanguageDef
sakuraCalcLangage =
  LanguageDef
    (unGenLanguageDef emptyDef)
      { nestedComments = false
      , caseSensitive = false
      }

tokenParser :: TokenParser
tokenParser = makeTokenParser sakuraCalcLangage

expression :: Parser String DSL.Expression
expression = fix \expr -> buildExprParser operatorTable (number <|> parens expr)
  where
  number = tokenParser.naturalOrFloat <#> case _ of
    Left i -> DSL.int i
    Right n -> DSL.float n
  parens = tokenParser.parens
  reservedOp = tokenParser.reservedOp
  operatorTable =
    [ [ Infix (reservedOp "^" $> DSL.pow) AssocLeft ]
    , [ Infix (reservedOp "/" $> DSL.div) AssocLeft ]
    , [ Infix (reservedOp "%" $> DSL.mod) AssocLeft ]
    , [ Infix (reservedOp "*" $> DSL.mul) AssocLeft ]
    , [ Infix (reservedOp "-" $> DSL.sub) AssocLeft ]
    , [ Infix (reservedOp "+" $> DSL.add) AssocLeft ]
    , [ Prefix (reservedOp "-" $> DSL.sub (DSL.int 0)) ]
    ]
