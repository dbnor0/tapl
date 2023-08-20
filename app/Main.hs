module Main where

import System.Directory

import Arith.Interpreter

main :: IO ()
main = do
  interpretSmallStep ".\\src\\arith\\arith"

