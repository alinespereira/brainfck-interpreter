module Brainfck.Program
  ( Instruction(..)
  , Program(..)
  , parseProgram
  )
where

import Control.Arrow (left)
import Data.Sequence (Seq)
import qualified Data.Sequence as S (fromList)
import Text.Printf (printf)

data Instruction
  = Increment
  | Decrement
  | Input
  | Output
  | JumpForward
  | JumpBackward
  | LoopStart
  | LoopEnd
  deriving (Eq, Show)

newtype Program = Program { getInstructions :: Seq Instruction }
  deriving (Eq, Show)

parseInstruction :: Char -> Either String Instruction
parseInstruction '+' = Right Increment
parseInstruction '-' = Right Decrement
parseInstruction ',' = Right Input
parseInstruction '.' = Right Output
parseInstruction '[' = Right LoopStart
parseInstruction ']' = Right LoopEnd
parseInstruction '>' = Right JumpForward
parseInstruction '<' = Right JumpBackward
parseInstruction ch  = Left (printf "invalid instruction %s" (show ch))

parseProgram :: String -> Either String Program
parseProgram = 
  fmap (Program . S.fromList) .
    sequenceA .
    zipWith (\i e -> left (prependPos i) e) [0 :: Int ..] .
    map parseInstruction
  where
    prependPos :: Int -> String -> String
    prependPos = printf "at position %d: %s"