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
type Exception = Text

keywords :: [Text]
keywords = ["if", "then", "else", "true", "false", "unit", "as", "let", "in", "nil", "cons", "isnil", "head", "tail"]

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
  , reserved "Atom" $> S.AtomTy
  , reserved "Unit" $> S.UnitTy
  , reserved "Num" $> S.NumTy
  ]

fn :: Parser S.Type
fn = S.FnTy <$> type'' <*> (reserved "->" *> type')

tupleTy :: Parser S.Type
tupleTy = S.TupleTy <$> between (reserved "{") (reserved "}") (sepBy type' (reserved ","))

listTy :: Parser S.Type
listTy = S.ListTy <$> (reserved "List" *> type')

type'' :: Parser S.Type
type'' = backtrack
  [ primitive
  , parens type'
  ]

type' :: Parser S.Type
type' = backtrack
  [ fn
  , tupleTy
  , listTy
  , primitive
  , parens type'
  ]

num :: Parser Term
num = S.NumT <$> lexeme decimal

bool :: Parser Term
bool = reserved "true" $> S.BoolT True <|> reserved "false" $> S.BoolT False

unit :: Parser Term
unit = reserved "unit" $> S.UnitT

tuple :: Parser Term
tuple = S.TupleT <$> between (reserved "{") (reserved "}") (sepBy term (reserved ","))

nil' :: Parser Term
nil' = S.NilT <$> (reserved "nil" *> between (reserved "[") (reserved "]") type')

cons' :: Parser Term
cons' = S.ConstT <$> (reserved "cons" *> between (reserved "[") (reserved "]") type') <*> term <*> (reserved "," *> term)

isnil :: Parser Term
isnil = S.IsNilT <$> (reserved "isnil" *> between (reserved "[") (reserved "]") type') <*> term

head' :: Parser Term
head' = S.HeadT <$> (reserved "head" *> between (reserved "[") (reserved "]") type') <*> term

tail' :: Parser Term
tail' = S.TailT <$> (reserved "tail" *> between (reserved "[") (reserved "]") type') <*> term

ops :: [[Operator Parser Term]]
ops =
    [ [ InfixL (reserved "." $> S.ProjectT) ]
    ,
      [ InfixL (reserved "*" $> S.ArithT S.Times)
      , InfixL (reserved "/" $> S.ArithT S.Divide) 
      ]
    , [ InfixL (reserved "+" $> S.ArithT S.Plus)
      , InfixL (reserved "-" $> S.ArithT S.Minus) 
      ]
    , [ InfixL (reserved ";" $> (S.AppT . S.AbsT "_" S.UnitTy))
      , InfixL app
      ]
    ]
    where app = pure S.AppT

as :: Parser Term
as = S.AsT <$> ascribable <*> (reserved "as" *> type')

if' :: Parser Term
if' = S.IfT <$> (reserved "if" *> term) <*> (reserved "then" *> term) <*> (reserved "else" *> term)

let' :: Parser Term
let' = S.LetT <$> (reserved "let" *> identifier) <*> (reserved "=" *> term) <*> (reserved "in" *> term)

term :: Parser Term
term = makeExprParser factor ops

factor :: Parser Term
factor = backtrack
    [ as
    , var
    , unit
    , bool
    , num
    , tuple
    , nil'
    , cons'
    , isnil
    , head'
    , tail'
    , if'
    , let'
    , abs
    , parens term
    ]

ascribable :: Parser Term
ascribable = backtrack
    [ parens term
    , var
    , unit
    , bool
    , num
    , tuple
    , nil'
    , cons'
    , isnil
    , head'
    , tail'
    , if'
    , let'
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