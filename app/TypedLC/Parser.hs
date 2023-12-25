{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TypedLC.Parser where

import TypedLC.Syntax qualified as S
import Common.Parser
import Data.Char
import Data.Text hiding (foldl, elem)
import Text.Megaparsec
import Prelude hiding (abs, seq)
import Data.Foldable (Foldable(foldr'))
import Data.Functor
import Control.Monad

type Term = S.Term Text
type Exception = Text

keywords :: [Text]
keywords = ["if", "then", "else", "true", "false", "unit"]

identifier :: Parser Text
identifier = do
  id <- lexeme $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum
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

if' :: Parser Term
if' = S.IfT <$> (reserved "if" *> term) <*> (reserved "then" *> term) <*> (reserved "else" *> term)

var :: Parser Term
var = S.VarT <$> identifier

abs :: Parser Term
abs = S.AbsT <$> (reserved "@" *> (identifier <|> wildcard)) <*> (reserved ":" *> type' <* reserved ".") <*> term

app :: Parser Term
app = do
  t1 <- termNoApp
  ts <- many1 termNoApp
  return $ foldl S.AppT t1 ts

termNoApp :: Parser Term
termNoApp
  = backtrack
    [ abs
    , var
    , bool
    , unit
    , if'
    , parens term
    ]

seq :: Parser Term
seq = do
  t <- termNoSeq
  reserved ";"
  ts <- sepBy termNoSeq (reserved ";")
  return $ foldl roll t ts
  where roll acc x = S.AppT (S.AbsT "_" S.UnitTy x) acc

termNoSeq :: Parser Term
termNoSeq
  = backtrack
    [ abs
    , app
    , var
    , bool
    , unit
    , if'
    , parens term
    ]

term :: Parser Term
term
  = backtrack
    [ app
    , abs
    , seq
    , bool
    , unit
    , if'
    , var
    , parens term
    ]

parse' :: Text -> Either Exception Term
parse' s =
  case runParser term "" s of
    Left err -> Left "Error parsing expression"
    Right t -> Right t