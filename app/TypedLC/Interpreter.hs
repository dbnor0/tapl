{-# LANGUAGE OverloadedStrings #-}

module TypedLC.Interpreter where
import TypedLC.Parser
import Data.Text.IO
import Prelude hiding (readFile)
import TypedLC.Eval.Nameless
import qualified Data.Set as Set
import Control.Monad.Reader
import TypedLC.Typecheck
import Control.Monad.Except
import TypedLC.Eval.SmallStep

interpret :: FilePath -> IO ()
interpret fp = do
  src <- readFile fp
  case parse' src of
    Left err -> print "Error parsing"
    Right t -> do
      let free = Set.elems $ runReader (getFree t) (Env Set.empty [])
          nameless = toNameless t
          tc = runReader (runExceptT (typecheck nameless)) (mkEnv (length free))
      print $ runReader (fromNameless nameless) (Env free [])
      case tc of
        Left err -> print err
        Right _ -> print $ runReader (fromNameless (eval nameless)) (Env free [])
      
