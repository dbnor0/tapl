module Main where

import System.Directory

import TypedLC.Interpreter

main :: IO ()
main = do
  interpret ".\\src\\typed\\atom"

