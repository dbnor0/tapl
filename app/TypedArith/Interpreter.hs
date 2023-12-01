{-# LANGUAGE ImportQualifiedPost #-}

module TypedArith.Interpreter where

import TypedArith.Parser
import TypedArith.Typecheck

import Data.Text.IO
import Prelude hiding (readFile)
import TypedArith.Eval.SmallStep qualified as SS (eval)

interpretSmallStep :: FilePath -> IO ()
interpretSmallStep fp = do
  src <- readFile fp
  case parse' src of
    Left err -> print "Error parsing"
    Right t -> do
      case typecheck t of
        Left err -> print err
        Right _ ->print $ SS.eval t