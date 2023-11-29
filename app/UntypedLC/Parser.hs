{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module UntypedLC.Parser where

import UntypedLC.Syntax qualified as S
import Common.Parser

import Data.Char
import Data.Text hiding (foldl)
import Text.Megaparsec
import Prelude hiding (abs)
import Data.Foldable (Foldable(foldr'))

type Term = S.Term ParseInfo Text

identifier :: Parser Text
identifier = lexeme $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum

var :: Parser Term
var = S.Var <$> getPos <*> identifier

abs :: Parser Term
abs = S.Abs <$> getPos <*> (reserved "@" *> identifier <* reserved ".") <*> term

app :: Parser Term
app = do
  p <- getPos
  t1 <- term'
  ts <- many1 term'
  return $ foldl (S.App p) t1 ts

term' :: Parser Term
term'
  = backtrack
    [ abs
    , var
    , parens term
    ]

term :: Parser Term
term
  = backtrack
    [ app
    , abs
    , var
    , parens term
    ]
