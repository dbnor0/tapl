{-# LANGUAGE OverloadedStrings #-}

module Effectful.Interpreter where
import Effectful.Parser
import Data.Text.IO
import Prelude hiding (readFile)
import Effectful.Eval.Nameless
import Effectful.Typecheck
import Effectful.Eval.SmallStep


interpret :: FilePath -> IO ()
interpret fp = do
  src <- readFile fp
  case parse' src of
    Left err -> print "Error parsing"
    Right t -> do
      let free = freeVars t
          nameless = toNameless t
          type' = tc nameless free
      print $ "Nameless: " <> show nameless
      print $ "Nameful: " <> show t
      case type' of
        Left err -> print $ "Typechecking error: " <> err
        Right _ -> print $ exec nameless