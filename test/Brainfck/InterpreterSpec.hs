module Brainfck.InterpreterSpec where

import SpecHelper
import Control.Monad.State.Lazy
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Brainfck.Program
import Brainfck.ProgramState
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
    , Output
    ]

programState :: ProgramState
programState = initializeProgram program

spec :: Spec
spec = do
    describe "Brainf*ck" $ do
        context "Interpreter" $ do
            it "runs a simple program to halt" $ do
                let p = initializeProgram program
                finalState <- execStateT runProgram p
                finalState `shouldSatisfy` isHalt