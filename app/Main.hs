module Main where

import System.Directory

import Effectful.Interpreter

main :: IO ()
main = do
  interpret ".\\src\\effectful\\r2r"

