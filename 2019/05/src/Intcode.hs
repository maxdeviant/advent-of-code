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

data Instruction
  = Add Parameter Parameter Int
  | Multiply Parameter Parameter Int
  | Input Parameter Int
  | Output Int
  | End
  deriving (Show)

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
