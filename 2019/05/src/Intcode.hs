module Intcode
  ( eval
  , parse
  ) where

import Data.List.Index (setAt)
import Data.List.Split (splitOn)

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

lastN :: Int -> [a] -> [a]
lastN n list = drop (length list - n) list

rightPad :: Int -> [Int] -> [Int]
rightPad n list = list ++ replicate (n - length list) 0

leftPad :: Int -> [Int] -> [Int]
leftPad n list = replicate (n - length list') 0 ++ list'
  where
    list' = take n list

data ParameterMode
  = PositionMode
  | ImmediateMode
  deriving (Show)

data Parameter =
  Parameter ParameterMode Int
  deriving (Show)

class Evaluate a where
  evaluate :: a -> [Int] -> (Int, [Int])

data AddInstruction =
  AddInstruction Parameter Parameter Int
  deriving (Show)

instance Evaluate AddInstruction where
  evaluate (AddInstruction paramA paramB outputPosition) program =
    (4, setAt outputPosition (inputA + inputB) program)
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data MultiplyInstruction =
  MultiplyInstruction Parameter Parameter Int
  deriving (Show)

instance Evaluate MultiplyInstruction where
  evaluate (MultiplyInstruction paramA paramB outputPosition) program =
    (4, setAt outputPosition (inputA * inputB) program)
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data InputInstruction =
  InputInstruction Parameter Int
  deriving (Show)

instance Evaluate InputInstruction where
  evaluate (InputInstruction _ _) program = (0, program)

data OutputInstruction =
  OutputInstruction Int
  deriving (Show)

instance Evaluate OutputInstruction where
  evaluate (OutputInstruction _) program = (0, program)

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
          Add addInstruction -> evaluate addInstruction
          Multiply multiplyInstruction -> evaluate multiplyInstruction
          Input inputInstruction -> evaluate inputInstruction
          Output outputInstruction -> evaluate outputInstruction
          End -> \program' -> (0, program')

readParameter :: Parameter -> [Int] -> Int
readParameter (Parameter PositionMode position) program = program !! position
readParameter (Parameter ImmediateMode value) _ = value

parseParameterMode :: Int -> ParameterMode
parseParameterMode 0 = PositionMode
parseParameterMode 1 = ImmediateMode
parseParameterMode _ = error "Invalid parameter mode."

eval :: [Int] -> [Int]
eval program = eval' 0 program
  where
    eval' instructionPointer instructions =
      case drop instructionPointer instructions of
        [] -> instructions
        99:_ -> snd $ evaluate End program
        opcode:rest ->
          let (opcode':_:modes) = rightPad 10 $ reverse $ digits opcode
              instruction =
                case opcode' of
                  1 ->
                    let (inputA:inputB:outputPosition:_) = rest
                        (modeA:modeB:_) = modes
                        paramA = Parameter (parseParameterMode modeA) inputA
                        paramB = Parameter (parseParameterMode modeB) inputB
                     in Add $ AddInstruction paramA paramB outputPosition
                  2 ->
                    let (inputA:inputB:outputPosition:_) = rest
                        (modeA:modeB:_) = modes
                        paramA = Parameter (parseParameterMode modeA) inputA
                        paramB = Parameter (parseParameterMode modeB) inputB
                     in Multiply $
                        MultiplyInstruction paramA paramB outputPosition
              (movePointer, program') = evaluate instruction program
           in eval' (instructionPointer + movePointer) program'

parse :: String -> [Int]
parse = map read . splitOn ","
