module Lib
  ( partOne
  ) where

import Data.List.Index (setAt)
import Data.List.Split (splitOn)

evalIntcode :: [Int] -> [Int]
evalIntcode program = evalIntcode' 0 program
  where
    evalIntcode' instructionPointer instructions =
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
           in evalIntcode'
                (instructionPointer + 4)
                (setAt outputPosition (operation inputA inputB) instructions)

restoreProgramAlarmState :: [Int] -> [Int]
restoreProgramAlarmState = setAt 2 2 . setAt 1 12

parseIntcode :: String -> [Int]
parseIntcode = map read . splitOn ","

partOne :: String -> Int
partOne = head . evalIntcode . restoreProgramAlarmState . parseIntcode
