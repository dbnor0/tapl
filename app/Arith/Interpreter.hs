{-# LANGUAGE ImportQualifiedPost #-}
module Arith.Interpreter where

import Arith.Parser

import Data.Text.IO
import Prelude hiding (readFile)
import Arith.Eval.SmallStep qualified as SS (eval) 

interpretSmallStep :: FilePath -> IO ()
interpretSmallStep fp = do
  src <- readFile fp
  case parse' src of
    Left err -> print "Error parsing"
    Right t -> print $ SS.eval t