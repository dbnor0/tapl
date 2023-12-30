{-# LANGUAGE OverloadedStrings #-}

module TypedLC.Interpreter where
import TypedLC.Parser
import Data.Text.IO
import Prelude hiding (readFile)
import qualified Data.Set as Set
import Control.Monad.Reader
import TypedLC.Typecheck
import Control.Monad.Except
import TypedLC.Eval.SmallStep
import TypedLC.Eval.Nameless (freeVars, fromNameless, toNameless)

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
        Left err -> print err
        Right _ -> print $ eval nameless