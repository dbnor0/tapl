{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Arith.Parser where

import Arith.Syntax qualified as S
import Common.Parser

import Data.Text
import Prelude hiding (succ, pred)
import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Foldable (Foldable(foldr'))

type Term = S.Term ParseInfo
type Exception = Text

trueLit :: Parser Term
trueLit = S.False' <$> getPos <* reserved "true"

falseLit :: Parser Term
falseLit = S.False' <$> getPos <* reserved "false"

zeroLit :: Parser Term
zeroLit = S.Zero <$> getPos <* reserved "0"

succ :: Parser Term
succ = S.Succ <$> getPos <*> (reserved "succ" *> term)

numericSucc :: Parser Term
numericSucc = do
  p <- getPos
  n <- lexeme decimal
  return $ foldr' (\_ acc -> S.Succ p acc) (S.Zero p) [1..n]

pred :: Parser Term
pred = S.Pred <$> getPos <*> (reserved "pred" *> term)

iszero :: Parser Term
iszero = S.IsZero <$> getPos <*> (reserved "iszero" *> term)

if' :: Parser Term
if' = S.If <$> getPos <*> (reserved "if" *> term) <*> (reserved "then" *> term) <*> (reserved "else" *> term)

term :: Parser Term
term
  =  parens term
  <|> backtrack
        [ trueLit
        , falseLit
        , zeroLit
        , succ
        , numericSucc
        , pred
        , iszero
        , if'
        ]

parse' :: Text -> Either Exception Term
parse' s =
  case runParser term "" s of
    Left err -> Left "Error parsing expression"
    Right t -> Right t