module Intcode
  ( eval
  , parse
  ) where

import Data.List.Index (setAt)
import Data.List.Split (splitOn)

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

rightPad :: Int -> [Int] -> [Int]
rightPad n list = list ++ replicate (n - length list) 0

data ParameterMode
  = PositionMode
  | ImmediateMode

data Parameter =
  Parameter ParameterMode Int

class IntcodeInstruction a where
  parameterCount :: a -> Int
  evaluate :: a -> [Int] -> ([Int], [IO ()])

data AddInstruction =
  AddInstruction Parameter Parameter Int

instance IntcodeInstruction AddInstruction where
  parameterCount _ = 3
  evaluate (AddInstruction paramA paramB outputPosition) program =
    (setAt outputPosition (inputA + inputB) program, [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data MultiplyInstruction =
  MultiplyInstruction Parameter Parameter Int

instance IntcodeInstruction MultiplyInstruction where
  parameterCount _ = 3
  evaluate (MultiplyInstruction paramA paramB outputPosition) program =
    (setAt outputPosition (inputA * inputB) program, [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data InputInstruction =
  InputInstruction Int Int

instance IntcodeInstruction InputInstruction where
  parameterCount _ = 1
  evaluate (InputInstruction input outputPosition) program =
    (setAt outputPosition input program, [])

data OutputInstruction =
  OutputInstruction Int (Int -> IO ())

instance IntcodeInstruction OutputInstruction where
  parameterCount _ = 1
  evaluate (OutputInstruction position writeOutput) program =
    (program, [writeOutput $ program !! position])

data Instruction
  = Add AddInstruction
  | Multiply MultiplyInstruction
  | Input InputInstruction
  | Output OutputInstruction
  | End [IO ()]

instance IntcodeInstruction Instruction where
  parameterCount instruction =
    case instruction of
      Add addInstruction -> parameterCount addInstruction
      Multiply multiplyInstruction -> parameterCount multiplyInstruction
      Input inputInstruction -> parameterCount inputInstruction
      Output outputInstruction -> parameterCount outputInstruction
      End _ -> 0
  evaluate instruction program = operation program
    where
      operation =
        case instruction of
          Add addInstruction -> evaluate addInstruction
          Multiply multiplyInstruction -> evaluate multiplyInstruction
          Input inputInstruction -> evaluate inputInstruction
          Output outputInstruction -> evaluate outputInstruction
          End outputs -> \program' -> (program', outputs)

readParameter :: Parameter -> [Int] -> Int
readParameter (Parameter PositionMode position) program = program !! position
readParameter (Parameter ImmediateMode value) _ = value

parseParameterMode :: Int -> ParameterMode
parseParameterMode 0 = PositionMode
parseParameterMode 1 = ImmediateMode
parseParameterMode _ = error "Invalid parameter mode."

eval :: (Int -> IO ()) -> [Int] -> ([Int], [IO ()])
eval writeOutput program = eval' 0 program []
  where
    eval' instructionPointer instructions outputs =
      case drop instructionPointer instructions of
        [] -> (instructions, outputs)
        99:_ -> evaluate (End outputs) instructions
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
                  3 ->
                    let (outputPosition:_) = rest
                        input = 1
                     in Input $ InputInstruction input outputPosition
                  4 ->
                    let (position:_) = rest
                     in Output $ OutputInstruction position writeOutput
                  invalidOpcode ->
                    error $ "Invalid opcode: " ++ show invalidOpcode
              (instructions', outputs') = evaluate instruction instructions
           in eval'
                (instructionPointer + 1 + parameterCount instruction)
                instructions'
                (outputs ++ outputs')

parse :: String -> [Int]
parse = map read . splitOn ","
