module Intcode
  ( eval
  , parse
  ) where

import Data.List.Index (setAt)
import Data.List.Split (splitOn)

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

data ParameterMode
  = PositionMode
  | ImmediateMode
  deriving (Show)

data Parameter =
  Parameter ParameterMode Int
  deriving (Show)

class Evaluate a where
  evaluate :: a -> [Int] -> [Int]

data AddInstruction =
  AddInstruction Parameter Parameter Int
  deriving (Show)

instance Evaluate AddInstruction where
  evaluate (AddInstruction paramA paramB output) program = program

data MultiplyInstruction =
  MultiplyInstruction Parameter Parameter Int
  deriving (Show)

instance Evaluate MultiplyInstruction where
  evaluate (MultiplyInstruction paramA paramB output) program = program

data InputInstruction =
  InputInstruction Parameter Int
  deriving (Show)

instance Evaluate InputInstruction where
  evaluate (InputInstruction _ _) program = program

data OutputInstruction =
  OutputInstruction Int
  deriving (Show)

instance Evaluate OutputInstruction where
  evaluate (OutputInstruction _) program = program

data Instruction
  = Add AddInstruction
  | Multiply MultiplyInstruction
  | Input InputInstruction
  | Output OutputInstruction
  | End
  deriving (Show)

instance Evaluate Instruction where
  evaluate instruction program = operation program
    where
      operation =
        case instruction of
          Add instruction -> evaluate instruction
          Multiply instruction -> evaluate instruction
          Input instruction -> evaluate instruction
          Output instruction -> evaluate instruction
          End -> id

readParameter :: Parameter -> [Int] -> Int
readParameter (Parameter PositionMode position) program = program !! position
readParameter (Parameter ImmediateMode value) _ = value

eval :: [Int] -> [Int]
eval program = eval' 0 program
  where
    eval' instructionPointer instructions =
      case drop instructionPointer instructions of
        [] -> instructions
        99:_ -> instructions
        opcode:positionA:positionB:outputPosition:_ ->
          let operation =
                case opcode of
                  1 -> (+)
                  2 -> (*)
              inputA = instructions !! positionA
              inputB = instructions !! positionB
           in eval'
                (instructionPointer + 4)
                (setAt outputPosition (operation inputA inputB) instructions)

parse :: String -> [Int]
parse = map read . splitOn ","
