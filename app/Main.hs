module Main where

import Control.Monad.State.Lazy
import Data.Char (ord)

import Brainfck.Interpreter
import Brainfck.Program

main :: IO ()
main = do
  let ps = replicate (ord 'a') '+' ++ "."
  let initialState = initializeProgram <$> parseProgram ps
  case initialState of
    Left err -> putStrLn $ "Error: " ++ err
    Right s -> do
      runStateT runProgram s
      return ()
