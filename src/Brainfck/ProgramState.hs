{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns#-}
module Brainfck.ProgramState where

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S

import Brainfck.Program

data ProgramState = ProgramState
  { getProgram :: Program
  , getIp :: Int
  , dataPointer :: Int
  , memory :: Seq Int
  , isHalt :: Bool
  } deriving (Show)

pattern DataPointer :: Int -> Seq Int -> ProgramState
pattern DataPointer d m <- ProgramState _ _ d m _

pattern InstructionPointer :: Seq Instruction -> Int -> ProgramState
pattern InstructionPointer is ip <- ProgramState (Program is) ip _ _ _

halt :: ProgramState -> ProgramState
halt p = p { isHalt = True }

jumpForward :: ProgramState -> ProgramState
jumpForward = updateDataPointerWith (+1)

jumpBackward :: ProgramState -> ProgramState
jumpBackward = updateDataPointerWith (`subtract` 1)

incrementData :: ProgramState -> ProgramState
incrementData = updateMemoryWith (+1)

decrementData :: ProgramState -> ProgramState
decrementData = updateMemoryWith (`subtract` 1)

incrementIp :: ProgramState -> ProgramState
incrementIp = updateIpWith (+1)

previousInstruction :: ProgramState -> ProgramState
previousInstruction = updateIpWith (`subtract` 1)

getData :: ProgramState -> Int
getData (DataPointer d m) = fromMaybe 0 (S.lookup d m)

setData :: Int -> ProgramState -> ProgramState
setData x st@(DataPointer d m) = st { memory = m' }
  where
    m' = S.update d x m


getInstruction :: ProgramState -> Maybe Instruction
getInstruction (InstructionPointer is ip) = 
  S.lookup ip is

updateDataPointerWith :: (Int -> Int) -> ProgramState -> ProgramState
updateDataPointerWith f st@(DataPointer d m) =
  let d' = f d
      dd = max 0 (d' + 1 - length m)
      m' = m >< S.replicate dd 0
    in st { dataPointer = d', memory = m' }

updateMemoryWith :: (Int -> Int) -> ProgramState -> ProgramState
updateMemoryWith f st@(DataPointer d m) =
  st { memory = S.adjust f d m }

updateIpWith :: (Int -> Int) -> ProgramState -> ProgramState
updateIpWith f st@(InstructionPointer _ ip) =
  st { getIp = f ip }