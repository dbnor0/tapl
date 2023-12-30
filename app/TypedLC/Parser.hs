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
import Text.Megaparsec.Char.Lexer hiding (lexeme)

type Term = S.Term Text
type Literal = S.Literal Text
type ListOp = S.ListOp Text
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
  , "nil"
  , "cons"
  , "isnil"
  , "head"
  , "tail"
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
  , reserved "Atom" $> S.AtomTy
  , reserved "Unit" $> S.UnitTy
  , reserved "Num" $> S.NumTy
  ]

fnType :: Parser S.Type
fnType = S.FnTy <$> nonFnType <*> (reserved "->" *> type')

tupleType :: Parser S.Type
tupleType = S.TupleTy <$> braces (sepBy type' comma)

listType :: Parser S.Type
listType = S.ListTy <$> (reserved "List" *> type')

nonFnType :: Parser S.Type
nonFnType = backtrack
  [ primitiveType
  , tupleType
  , listType
  , parens type'
  ]

type' :: Parser S.Type
type' = backtrack
  [ fnType
  , primitiveType
  , tupleType
  , listType
  , parens type'
  ]

listAnnotation :: Parser S.Type
listAnnotation = brackets type'

-- literals

boolLit :: Parser Literal
boolLit =   reserved "true" $> S.BoolL True
        <|> reserved "false" $> S.BoolL False

numLit :: Parser Literal
numLit = S.NumL <$> lexeme decimal

-- string

tupleLit :: Parser Literal
tupleLit = S.TupleL <$> braces (sepBy term comma)

nilLit :: Parser Literal
nilLit = S.NilL <$> (reserved "nil" *> brackets type')

consLit :: Parser Literal
consLit =   S.ConsL
        <$> (reserved "cons" *> brackets type')
        <*> term
        <*> (comma *> term)

unitLit :: Parser Literal
unitLit = reserved "unit" $> S.UnitL

-- operators

isnil :: Parser ListOp
isnil = S.IsNil <$> (reserved "isnil" *> brackets type') <*> term

head' :: Parser ListOp
head' = S.Head <$> (reserved "head" *> brackets type') <*> term

tail' :: Parser ListOp
tail' = S.Tail <$> (reserved "tail" *> brackets type') <*> term

ops :: [[Operator Parser Term]]
ops =
    [ [ InfixL (reserved "*" $> S.ArithT S.Times)
      , InfixL (reserved "/" $> S.ArithT S.Divide)
      ]
    , [ InfixL (reserved "+" $> S.ArithT S.Plus)
      , InfixL (reserved "-" $> S.ArithT S.Minus)
      ]
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
  , tupleLit
  , nilLit
  , consLit
  , unitLit
  ]

varTerm :: Parser Term
varTerm = S.VarT <$> identifier

listTerm :: Parser Term
listTerm = S.ListT <$> backtrack
  [ isnil
  , head'
  , tail'
  ]

projectTerm :: Parser Term
projectTerm = do
  t <- projectable <* period
  ps <- sepBy selector period
  return $ foldr' (flip S.ProjectT) t ps

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

absTerm :: Parser Term
absTerm =   S.AbsT
        <$> (reserved "@" *> binder)
        <*> (colon *> type' <* period)
        <*> term
  where binder = identifier <|> wildcard

factor :: Parser Term
factor = backtrack
    [ ascriptionTerm
    , varTerm
    , projectTerm
    , litTerm
    , listTerm
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
    , listTerm
    , ifTerm
    , letTerm
    , absTerm
    , parens term
    ]

parse' :: Text -> Either Exception Term
parse' s =
  case runParser term "" s of
    Left err -> Left "Error parsing term"
    Right t -> Right t