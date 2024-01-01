{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Effectful.Parser where

import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Effectful.Syntax qualified as S
import Common.Parser
import Data.Char
import Data.Text hiding (foldl, foldl', elem)
import Text.Megaparsec
import Prelude hiding (abs, seq)
import Data.Foldable (Foldable(foldr', foldl'))
import Data.Functor
import Control.Monad
import Text.Megaparsec.Char.Lexer hiding (lexeme)
import Data.Map hiding (foldr', foldl')

type Term = S.Term Text
type Literal = S.Literal Text
type Exception = Text

-- utils

braces :: Parser a -> Parser a
braces = between (reserved "{") (reserved "}")

brackets :: Parser a -> Parser a
brackets = between (reserved "[") (reserved "]")

comma :: Parser ()
comma = reserved ","

period :: Parser ()
period = reserved "."

colon :: Parser ()
colon = reserved ":"

-- keywords & identifiers

keywords :: [Text]
keywords =
  [ "if"
  , "then"
  , "else"
  , "true"
  , "false"
  , "unit"
  , "cast"
  , "as"
  , "let"
  , "in"
  , "ref"
  ]

identifier :: Parser Text
identifier = do
  id <- lexeme $ cons <$> satisfy isAlpha <*> takeWhileP Nothing isAlphaNum
  when (id `elem` keywords)
    (fail "keyword")
  return id

selector :: Parser Text
selector = pack . show <$> lexeme decimal <|> identifier

wildcard :: Parser Text
wildcard = lexeme "_"

-- types

primitiveType :: Parser S.Type
primitiveType = backtrack
  [ reserved "Bool" $> S.BoolTy
  , reserved "Unit" $> S.UnitTy
  , reserved "Num" $> S.NumTy
  ]

fnType :: Parser S.Type
fnType = S.FnTy <$> nonFnType <*> (reserved "->" *> type')

recordType :: Parser S.Type
recordType = S.RecordTy . fromList <$> braces (sepBy field comma)
  where field = (,) <$> (identifier <* colon) <*> type'

refType :: Parser S.Type
refType = S.RefTy <$> (reserved "Ref" *> type')

nonFnType :: Parser S.Type
nonFnType = backtrack
  [ primitiveType
  , recordType
  , refType
  , parens type'
  ]

type' :: Parser S.Type
type' = backtrack
  [ fnType
  , primitiveType
  , recordType
  , refType
  , parens type'
  ]

-- literals

boolLit :: Parser Literal
boolLit =   reserved "true" $> S.BoolL True
        <|> reserved "false" $> S.BoolL False

numLit :: Parser Literal
numLit = S.NumL <$> lexeme decimal

recordLit :: Parser Literal
recordLit = S.RecordL . fromList <$> braces (sepBy field comma)
  where field = (,) <$> (identifier <* reserved "=") <*> term

unitLit :: Parser Literal
unitLit = reserved "unit" $> S.UnitL

-- operators

ops :: [[Operator Parser Term]]
ops =
    [ [ Prefix (reserved "!" $> S.DerefT) ]
    , [ InfixL (reserved "*" $> S.ArithT S.Times)
      , InfixL (reserved "/" $> S.ArithT S.Divide)
      ]
    , [ InfixL (reserved "+" $> S.ArithT S.Plus)
      , InfixL (reserved "-" $> S.ArithT S.Minus)
      ]
    , [ InfixL (reserved ":=" $> S.AssignT) ]
    , [ InfixL (reserved ";" $> (\t1 t2 -> S.AppT (S.AbsT "_" S.UnitTy t2) t1))
      , InfixL app
      ]
    ]
    where app = pure S.AppT

-- terms

term :: Parser Term
term = makeExprParser factor ops

litTerm :: Parser Term
litTerm = S.LitT <$> backtrack
  [ boolLit
  , numLit
  , recordLit
  , unitLit
  ]

varTerm :: Parser Term
varTerm = S.VarT <$> identifier

projectTerm :: Parser Term
projectTerm = do
  t <- projectable <* period
  ps <- sepBy selector period
  return $ foldl' S.ProjectT t ps

ifTerm :: Parser Term
ifTerm =   S.IfT
       <$> (reserved "if" *> term)
       <*> (reserved "then" *> term)
       <*> (reserved "else" *> term)

letTerm :: Parser Term
letTerm =   S.LetT
        <$> (reserved "let" *> identifier)
        <*> (reserved "=" *> term)
        <*> (reserved "in" *> term)

ascriptionTerm :: Parser Term
ascriptionTerm = S.AsT <$> (reserved "cast" *> term) <*> (reserved "as" *> type')

refTerm :: Parser Term
refTerm = S.RefT <$> (reserved "ref" *> term)

absTerm :: Parser Term
absTerm =   S.AbsT
        <$> (reserved "@" *> binder)
        <*> (colon *> type' <* period)
        <*> term
  where binder = identifier <|> wildcard

factor :: Parser Term
factor = backtrack
    [ refTerm
    , ascriptionTerm
    , varTerm
    , projectTerm
    , litTerm
    , ifTerm
    , letTerm
    , absTerm
    , parens term
    ]

-- auxiliaries

projectable :: Parser Term
projectable = backtrack
    [ ascriptionTerm
    , varTerm
    , litTerm
    , ifTerm
    , letTerm
    , refTerm
    , absTerm
    , parens term
    ]

parse' :: Text -> Either Exception Term
parse' s =
  case runParser term "" s of
    Left err -> Left "Error parsing term"
    Right t -> Right t