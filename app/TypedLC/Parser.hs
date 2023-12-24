{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TypedLC.Parser where

import TypedLC.Syntax qualified as S
import Common.Parser
import Data.Char
import Data.Text hiding (foldl, elem)
import Text.Megaparsec
import Prelude hiding (abs)
import Data.Foldable (Foldable(foldr'))
import Data.Functor
import Control.Monad

type Term = S.Term Text
type Exception = Text

keywords :: [Text]
keywords = ["if", "then", "else", "true", "false"]

identifier :: Parser Text
identifier = do
  id <- lexeme $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum
  when (id `elem` keywords) 
    (fail "keyword")
  return id

primitive :: Parser S.Type
primitive = backtrack
  [ reserved "Bool" $> S.BoolTy
  , reserved "A" $> S.AtomTy
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

if' :: Parser Term
if' = S.IfT <$> (reserved "if" *> term) <*> (reserved "then" *> term) <*> (reserved "else" *> term)

var :: Parser Term
var = S.VarT <$> identifier

abs :: Parser Term
abs = S.AbsT <$> (reserved "@" *> identifier) <*> (reserved ":" *> type' <* reserved ".") <*> term

app :: Parser Term
app = do
  t1 <- term'
  ts <- many1 term'
  return $ foldl S.AppT t1 ts

term' :: Parser Term
term'
  = backtrack
    [ abs
    , var
    , bool
    , if'
    , parens term
    ]

term :: Parser Term
term
  = backtrack
    [ app
    , abs
    , bool
    , if'
    , var
    , parens term
    ]

parse' :: Text -> Either Exception Term
parse' s =
  case runParser term "" s of
    Left err -> Left "Error parsing expression"
    Right t -> Right t