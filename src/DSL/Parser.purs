module Macro.DSL.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (fromFoldable)
import Data.Either (Either)
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
      , reservedNames =
        [ "sum"
        , "avg"
        , "abs"
        , "round"
        , "floor"
        , "ceil"
        , "sqrt"
        , "log"
        ]
      }

tokenParser :: TokenParser
tokenParser = makeTokenParser sakuraCalcLangage

expression :: Parser String DSL.Expression
expression = fix \expr -> buildExprParser operatorTable $ number <|> funcApply expr <|> parens expr
  where
  number = DSL.numeric <$> tokenParser.naturalOrFloat

  arrayFunc e =
    (reserved "sum" *> (DSL.Sum <$> e))
      <|> (reserved "avg" *> (DSL.Avg <$> e))

  valueFunc e =
    (reserved "abs" *> (DSL.Abs <$> e))
      <|> (reserved "round" *> (DSL.Round <$> e))
      <|> (reserved "floor" *> (DSL.Floor <$> e))
      <|> (reserved "ceil" *> (DSL.Ceil <$> e))
      <|> (reserved "sqrt" *> (DSL.Sqrt <$> e))
      <|> (reserved "log" *> (DSL.Log <$> e))

  funcApply e =
    map DSL.FuncApplyExpr
      $ arrayFunc (brackets $ fromFoldable <$> commaSep e)
      <|> valueFunc e

  brackets = tokenParser.brackets

  commaSep = tokenParser.commaSep

  parens = tokenParser.parens

  reserved = tokenParser.reserved

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
