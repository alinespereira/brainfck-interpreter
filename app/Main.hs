module Main where

import Control.Monad.State.Lazy
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import System.IO

import Brainfck.Program
import Brainfck.Interpreter


program :: Program
program = Program instructions

instructions :: Seq Instruction
instructions = S.fromList 
    [ JumpForward
    , JumpForward
    , JumpForward
    , Increment
    , Increment
    , Input
    , Output
    ]

ps = initializeProgram program

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  evalStateT runProgram ps
  putStrLn ""
