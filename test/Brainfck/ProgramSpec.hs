module Brainfck.ProgramSpec where

import SpecHelper

import Data.Char
import Data.Either (isLeft, isRight)

import Brainfck.Program

spec :: Spec
spec = do
  describe "Brainfck.Program" $ do
    context "valid instruction chars" $ do
      prop "should be able to parse a program" $ do
        forAll validInstructionSetGen $ do
          \instructions ->
            parseProgram instructions `shouldSatisfy` isRight
    
    context "invalid instruction chars" $ do
      prop "should be able to fail parsing" $ do
        forAll invalidInstructionSetGen $ do
          \instructions ->
            parseProgram instructions `shouldSatisfy` isLeft

instructionChars :: [Char]
instructionChars = ['+', '-', '<', '>', '.', ',', '[', ']']

invalidChars :: [Char]
invalidChars = filter (`notElem` instructionChars) $ map chr [0 .. 128]

charSetGen :: Int -> [Char] -> Gen [Char]
charSetGen size = vectorOf size . elements

validInstructionSetGen :: Gen [Char]
validInstructionSetGen = do
  size <- choose (1, 50)
  charSetGen size instructionChars

invalidInstructionSetGen :: Gen [Char]
invalidInstructionSetGen = do
  size <- choose (1, 50)
  charSetGen size invalidChars
