{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module TypedArith.Parser where

import TypedArith.Syntax qualified as S
import Common.Parser

import Data.Text
import Prelude hiding (succ, pred)
import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Foldable (Foldable(foldr'))

type Exception = Text

trueLit :: Parser S.Term
trueLit = S.False' <$ reserved "true"

falseLit :: Parser S.Term
falseLit = S.False' <$ reserved "false"

zeroLit :: Parser S.Term
zeroLit = S.Zero <$ reserved "0"

succ :: Parser S.Term
succ = S.Succ <$> (reserved "succ" *> term)

numericSucc :: Parser S.Term
numericSucc = do
  n <- lexeme decimal
  return $ foldr' (\_ acc -> S.Succ acc) S.Zero [1..n]

pred :: Parser S.Term
pred = S.Pred <$> (reserved "pred" *> term)

iszero :: Parser S.Term
iszero = S.IsZero <$> (reserved "iszero" *> term)

if' :: Parser S.Term
if' = S.If <$> (reserved "if" *> term) <*> (reserved "then" *> term) <*> (reserved "else" *> term)

term :: Parser S.Term
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

parse' :: Text -> Either Exception S.Term
parse' s =
  case runParser term "" s of
    Left err -> Left "Error parsing expression"
    Right t -> Right t