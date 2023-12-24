{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Parser where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as P
import Text.Megaparsec.Char qualified as P hiding (space)
import Control.Monad
import Text.Megaparsec.Char.Lexer hiding (lexeme)
import Data.Functor

type Parser a = Parsec Void Text a

type SourceFile = String
type Line = Int
type Column = Int

data ParseInfo = ParseInfo SourceFile Line Column
  deriving stock (Eq, Show)

getPos :: Parser ParseInfo
getPos = do
  sourcePos <- getSourcePos
  let sourceFile = sourceName sourcePos
  let line = unPos . sourceLine $ sourcePos
  let column = unPos . sourceLine $ sourcePos
  return $ ParseInfo sourceFile line column

lineComment :: Parser ()
lineComment = P.skipLineComment "//"

blockComment :: Parser ()
blockComment = P.skipBlockComment "/*" "*/"

spaceOrComment :: Parser ()
spaceOrComment = P.space P.space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = P.lexeme spaceOrComment

reserved :: Text -> Parser ()
reserved = void . lexeme . chunk

parens :: Parser a -> Parser a
parens = between (reserved "(") (reserved ")")

backtrack :: [Parser a] -> Parser a
backtrack = choice . (<$>) try

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

operator :: Text -> (a -> a -> a) -> Parser (a -> a -> a)
operator opStr op = reserved opStr $> op

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = do
      maybeOp <- optional op
      case maybeOp of
        Nothing -> return x
        Just f  -> do
          y <- p
          rest (f x y)