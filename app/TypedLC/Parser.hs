{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TypedLC.Parser where

import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import TypedLC.Syntax qualified as S
import Common.Parser
import Data.Char
import Data.Text hiding (foldl, elem)
import Text.Megaparsec
import Prelude hiding (abs, seq)
import Data.Foldable (Foldable(foldr'))
import Data.Functor
import Control.Monad
import Text.ParserCombinators.ReadP

type Term = S.Term Text
type Exception = Text

keywords :: [Text]
keywords = ["if", "then", "else", "true", "false", "unit", "as"]

identifier :: Parser Text
identifier = do
  id <- lexeme $ cons <$> Text.Megaparsec.satisfy isAlpha <*> takeWhileP Nothing isAlphaNum
  when (id `elem` keywords)
    (fail "keyword")
  return id

wildcard :: Parser Text
wildcard = lexeme "_"

primitive :: Parser S.Type
primitive = backtrack
  [ reserved "Bool" $> S.BoolTy
  , reserved "A" $> S.AtomTy
  , reserved "Unit" $> S.UnitTy
  ]

fn :: Parser S.Type
fn = S.FnTy <$> type'' <*> (reserved "->" *> type')

type'' :: Parser S.Type
type'' = backtrack
  [ primitive
  , parens type'
  ]

type' :: Parser S.Type
type' = backtrack
  [ fn
  , primitive
  , parens type'
  ]

bool :: Parser Term
bool = reserved "true" $> S.BoolT True <|> reserved "false" $> S.BoolT False

unit :: Parser Term
unit = reserved "unit" $> S.UnitT

ops :: [[Operator Parser Term]]
ops =
    [ [ InfixL (reserved ";" $> (S.AppT . S.AbsT "_" S.UnitTy))
      , InfixL app
      ]
    ]
    where app = pure S.AppT

as :: Parser Term
as = S.AsT <$> factor' <*> (reserved "as" *> type')

if' :: Parser Term
if' = S.IfT <$> (reserved "if" *> term) <*> (reserved "then" *> term) <*> (reserved "else" *> term)

term :: Parser Term
term = makeExprParser factor ops

factor :: Parser Term
factor = backtrack
    [ as
    , var
    , unit
    , bool
    , if'
    , abs
    , parens term
    ]

factor' :: Parser Term
factor' = backtrack
    [ parens term
    , var
    , unit
    , bool
    , if'
    , abs
    ]

var :: Parser Term
var = S.VarT <$> identifier

abs :: Parser Term
abs = S.AbsT <$> (reserved "@" *> (identifier <|> wildcard)) <*> (reserved ":" *> type' <* reserved ".") <*> term

parse' :: Text -> Either Exception Term
parse' s =
  case runParser term "" s of
    Left err -> Left "Error parsing expression"
    Right t -> Right t