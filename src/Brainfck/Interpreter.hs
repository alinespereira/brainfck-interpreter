module Brainfck.Interpreter where

import Control.Monad.State.Lazy
import Data.Char (chr, ord)
import qualified Data.Sequence as S

import Brainfck.Program
import Brainfck.ProgramState

type ProgramStateT = StateT ProgramState IO

initializeProgram :: Program -> ProgramState
initializeProgram p = ProgramState 
  { getProgram = p 
  , getIp = 0
  , dataPointer = 0
  , memory = S.singleton 0
  , isHalt = S.null . getInstructions $ p
  }


runProgram :: ProgramStateT ()
runProgram = do
  runInstruction
  st <- get
  unless (isHalt st) $ do
    runIncrementIp
    runProgram


runInstruction :: ProgramStateT ()
runInstruction = do
  programState <- get
  case getInstruction programState of
    Nothing -> runHalt
    Just instruction -> case instruction of
      Increment -> runIncrement
      Decrement -> runDecrement
      Input -> runInput
      Output -> runOutput
      JumpForward -> runJumpForward
      JumpBackward -> runJumpBackward
      LoopStart -> runLoopStart
      LoopEnd -> runLoopEnd

runJumpForward :: ProgramStateT ()
runJumpForward = modify jumpForward

runJumpBackward :: ProgramStateT ()
runJumpBackward = modify jumpBackward

runIncrement :: ProgramStateT ()
runIncrement = modify incrementData

runDecrement :: ProgramStateT ()
runDecrement = modify decrementData

runLoopStart :: ProgramStateT ()
runLoopStart = undefined

runLoopEnd :: ProgramStateT ()
runLoopEnd = undefined

runInput :: ProgramStateT ()
runInput = do
  liftIO $ putStr "Enter a character: "
  ch:_ <- liftIO getLine
  liftIO $ putStrLn $ "Provided value is " ++ show ch
  st <- fmap (setData (ord ch)) get
  put st

runOutput :: ProgramStateT ()
runOutput = do
  st <- get
  liftIO $ putChar (chr (getData st))

runIncrementIp :: ProgramStateT ()
runIncrementIp = modify incrementIp

runHalt :: ProgramStateT ()
runHalt = modify halt
