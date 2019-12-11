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

data UpdateInstructionPointer
  = MoveInstructionPointer Int
  | SetInstructionPointer Int

class IntcodeInstruction a where
  evaluate :: a -> [Int] -> (UpdateInstructionPointer, [Int], [IO ()])

data AddInstruction =
  AddInstruction Parameter Parameter Int

instance IntcodeInstruction AddInstruction where
  evaluate (AddInstruction paramA paramB outputPosition) program =
    ( MoveInstructionPointer 4
    , setAt outputPosition (inputA + inputB) program
    , [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data MultiplyInstruction =
  MultiplyInstruction Parameter Parameter Int

instance IntcodeInstruction MultiplyInstruction where
  evaluate (MultiplyInstruction paramA paramB outputPosition) program =
    ( MoveInstructionPointer 4
    , setAt outputPosition (inputA * inputB) program
    , [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data InputInstruction =
  InputInstruction Int Int

instance IntcodeInstruction InputInstruction where
  evaluate (InputInstruction input outputPosition) program =
    (MoveInstructionPointer 2, setAt outputPosition input program, [])

data OutputInstruction =
  OutputInstruction Int (Int -> IO ())

instance IntcodeInstruction OutputInstruction where
  evaluate (OutputInstruction position writeOutput) program =
    (MoveInstructionPointer 2, program, [writeOutput $ program !! position])

data Instruction
  = Add AddInstruction
  | Multiply MultiplyInstruction
  | Input InputInstruction
  | Output OutputInstruction

instance IntcodeInstruction Instruction where
  evaluate instruction program = operation program
    where
      operation =
        case instruction of
          Add addInstruction -> evaluate addInstruction
          Multiply multiplyInstruction -> evaluate multiplyInstruction
          Input inputInstruction -> evaluate inputInstruction
          Output outputInstruction -> evaluate outputInstruction

readParameter :: Parameter -> [Int] -> Int
readParameter (Parameter PositionMode position) program = program !! position
readParameter (Parameter ImmediateMode value) _ = value

parseParameterMode :: Int -> ParameterMode
parseParameterMode 0 = PositionMode
parseParameterMode 1 = ImmediateMode
parseParameterMode _ = error "Invalid parameter mode."

eval :: (Int -> IO ()) -> [Int] -> ([Int], [IO ()])
eval writeOutput program =
  let (program', outputs) = eval' 0 program []
   in (program', outputs)
  where
    eval' instructionPointer instructions outputs =
      case drop instructionPointer instructions of
        [] -> (instructions, outputs)
        99:_ -> (instructions, outputs)
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
              (updateInstructionPointer, instructions', outputs') =
                evaluate instruction instructions
           in eval'
                (case updateInstructionPointer of
                   MoveInstructionPointer delta -> instructionPointer + delta
                   SetInstructionPointer pointer -> pointer)
                instructions'
                (outputs ++ outputs')

parse :: String -> [Int]
parse = map read . splitOn ","
