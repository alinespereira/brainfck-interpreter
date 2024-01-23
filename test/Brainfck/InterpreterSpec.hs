module Brainfck.InterpreterSpec where

import SpecHelper
import Control.Monad.State.Lazy
import Data.Sequence (Seq)
import qualified Data.Sequence as S

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

execStateT runProgram ps

-- spec :: Spec
-- spec = do
--     describe "" $ do
--         context "" $ do
--             it "" $ do
--                 let p = initializeProgram program
--                 execStateT runProgram p `sohuldSatisfy` isHalt