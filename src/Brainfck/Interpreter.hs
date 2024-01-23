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
  programState <- get
  case getInstruction programState of
    Nothing -> return ()
    Just instruction ->
      let programState' = 
        case instruction of
          Increment -> incrementData programState
          Decrement -> decrementData programState
          Input -> undefined
          Output -> undefined
          JumpForward -> jumpForward programState
          JumpBackward -> jumpBackward programState
          LoopStart -> undefined
          LoopEnd -> undefined
      put programState'
      runProgram

-- runProgram :: ProgramStateT ()
-- runProgram = do
--   programState <- get
--   case getInstruction programState of
--     Nothing -> return ()
--     Just i -> do
--       runInstruction i
--       runProgram

-- runInstruction :: Instruction -> ProgramStateT (Maybe Instruction)

-- runInstruction JumpForward = do
--   st <- fmap (nextInstruction . jumpForward) get
--   put st
--   return (getInstruction st)

-- runInstruction JumpBackward =  do
--   st <- fmap (nextInstruction . jumpBackward) get
--   put st
--   return (getInstruction st)

-- runInstruction Increment = do
--   st <- fmap (nextInstruction . incrementData) get
--   put st
--   return (getInstruction st)

-- runInstruction Decrement = do
--   st <- fmap (nextInstruction . decrementData) get
--   put st
--   return (getInstruction st)

-- runInstruction LoopStart = undefined
-- runInstruction LoopEnd = undefined
-- runInstruction Input = do
--   liftIO $ putStr "Enter a character: "
--   ch:_ <- liftIO $ getLine
--   liftIO $ putStrLn $ "Provided value is " ++ (show ch)
--   st <- fmap (nextInstruction . setData (ord ch)) get
--   put st
--   return (getInstruction st)

-- runInstruction Output =  do
--   st <- get
--   liftIO $ putChar (chr (getData st))
--   let st' = nextInstruction st
--   put st'
--   return (getInstruction st')

jumpForward :: ProgramState -> ProgramState
jumpForward = updateDataPointerWith (+1)

jumpBackward :: ProgramState -> ProgramState
jumpBackward = updateDataPointerWith (`subtract` 1)

incrementData :: ProgramState -> ProgramState
incrementData = updateMemoryWith (+1)

decrementData :: ProgramState -> ProgramState
decrementData = updateMemoryWith (`subtract` 1)

nextInstruction :: ProgramState -> ProgramState
nextInstruction = updateInstructionPointerWith (+1)

previousInstruction :: ProgramState -> ProgramState
previousInstruction = updateInstructionPointerWith (`subtract` 1)

getData :: ProgramState -> Int
getData (ProgramState _ _ d m) = fromMaybe 0 (S.lookup d m)

setData :: Int -> ProgramState -> ProgramState
setData x st@(ProgramState _ _ d m) = st { memory = m' }
  where
    m' = S.update d x m

getInstruction :: ProgramState -> Maybe Instruction
getInstruction (ProgramState (Program is) ip _ _) = 
  S.lookup ip is

updateDataPointerWith :: (Int -> Int) -> ProgramState -> ProgramState
updateDataPointerWith f st@(ProgramState _ _ d m) =
  let d' = f d
      dd = max 0 (d' - length m)
      m' = m >< S.replicate dd 0
    in st { dataPointer = d', memory = m' }

updateMemoryWith :: (Int -> Int) -> ProgramState -> ProgramState
updateMemoryWith f st@(ProgramState _ _ d m) =
  st { memory = S.adjust f d m }

updateInstructionPointerWith :: (Int -> Int) -> ProgramState -> ProgramState
updateInstructionPointerWith f st@(ProgramState _ i _ _) =
  st { instructionPointer = f i }