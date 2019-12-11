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

data JumpIfTrueInstruction =
  JumpIfTrueInstruction Parameter Parameter

instance IntcodeInstruction JumpIfTrueInstruction where
  evaluate (JumpIfTrueInstruction paramA paramB) program =
    ( if inputA /= 0
        then SetInstructionPointer inputB
        else MoveInstructionPointer 3
    , program
    , [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data JumpIfFalseInstruction =
  JumpIfFalseInstruction Parameter Parameter

instance IntcodeInstruction JumpIfFalseInstruction where
  evaluate (JumpIfFalseInstruction paramA paramB) program =
    ( if inputA == 0
        then SetInstructionPointer inputB
        else MoveInstructionPointer 3
    , program
    , [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data LessThanInstruction =
  LessThanInstruction Parameter Parameter Int

instance IntcodeInstruction LessThanInstruction where
  evaluate (LessThanInstruction paramA paramB outputPosition) program =
    ( MoveInstructionPointer 4
    , setAt
        outputPosition
        (if inputA < inputB
           then 1
           else 0)
        program
    , [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data EqualsInstruction =
  EqualsInstruction Parameter Parameter Int

instance IntcodeInstruction EqualsInstruction where
  evaluate (EqualsInstruction paramA paramB outputPosition) program =
    ( MoveInstructionPointer 4
    , setAt
        outputPosition
        (if inputA == inputB
           then 1
           else 0)
        program
    , [])
    where
      inputA = readParameter paramA program
      inputB = readParameter paramB program

data Instruction
  = Add AddInstruction
  | Multiply MultiplyInstruction
  | Input InputInstruction
  | Output OutputInstruction
  | JumpIfTrue JumpIfTrueInstruction
  | JumpIfFalse JumpIfFalseInstruction
  | LessThan LessThanInstruction
  | Equals EqualsInstruction

instance IntcodeInstruction Instruction where
  evaluate instruction program = operation program
    where
      operation =
        case instruction of
          Add addInstruction -> evaluate addInstruction
          Multiply multiplyInstruction -> evaluate multiplyInstruction
          Input inputInstruction -> evaluate inputInstruction
          Output outputInstruction -> evaluate outputInstruction
          JumpIfTrue jumpIfTrueInstruction -> evaluate jumpIfTrueInstruction
          JumpIfFalse jumpIfFalseInstruction -> evaluate jumpIfFalseInstruction
          LessThan lessThanInstruction -> evaluate lessThanInstruction
          Equals equalsInstruction -> evaluate equalsInstruction

readParameter :: Parameter -> [Int] -> Int
readParameter (Parameter PositionMode position) program = program !! position
readParameter (Parameter ImmediateMode value) _ = value

parseParameterMode :: Int -> ParameterMode
parseParameterMode 0 = PositionMode
parseParameterMode 1 = ImmediateMode
parseParameterMode _ = error "Invalid parameter mode."

eval :: Int -> (Int -> IO ()) -> [Int] -> ([Int], [IO ()])
eval programInput writeOutput program =
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
                        input = programInput
                     in Input $ InputInstruction input outputPosition
                  4 ->
                    let (position:_) = rest
                     in Output $ OutputInstruction position writeOutput
                  5 ->
                    let (inputA:inputB:_) = rest
                        (modeA:modeB:_) = modes
                        paramA = Parameter (parseParameterMode modeA) inputA
                        paramB = Parameter (parseParameterMode modeB) inputB
                     in JumpIfTrue $ JumpIfTrueInstruction paramA paramB
                  6 ->
                    let (inputA:inputB:_) = rest
                        (modeA:modeB:_) = modes
                        paramA = Parameter (parseParameterMode modeA) inputA
                        paramB = Parameter (parseParameterMode modeB) inputB
                     in JumpIfFalse $ JumpIfFalseInstruction paramA paramB
                  7 ->
                    let (inputA:inputB:inputC:_) = rest
                        (modeA:modeB:_) = modes
                        paramA = Parameter (parseParameterMode modeA) inputA
                        paramB = Parameter (parseParameterMode modeB) inputB
                     in LessThan $ LessThanInstruction paramA paramB inputC
                  8 ->
                    let (inputA:inputB:inputC:_) = rest
                        (modeA:modeB:_) = modes
                        paramA = Parameter (parseParameterMode modeA) inputA
                        paramB = Parameter (parseParameterMode modeB) inputB
                     in Equals $ EqualsInstruction paramA paramB inputC
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
