module Main where

import Prelude
import Data.Array (uncons)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Int.Bits as Bits
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)
import Data.String (Pattern(..), split, trim)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype Wire
  = Wire String

derive instance newtypeWire :: Newtype Wire _

derive instance eqWire :: Eq Wire

derive instance ordWire :: Ord Wire

newtype Signal
  = Signal Int

derive instance newtypeSignal :: Newtype Signal _

and :: Signal -> Signal -> Signal
and = over2 Signal Bits.and

or :: Signal -> Signal -> Signal
or = over2 Signal Bits.or

lshift :: Signal -> Signal -> Signal
lshift = over2 Signal Bits.shl

rshift :: Signal -> Signal -> Signal
rshift = over2 Signal Bits.shr

complement :: Signal -> Signal
complement = over Signal Bits.complement

data Source
  = WireSource Wire
  | SignalSource Signal

data Instruction
  = Input Source Wire
  | And Source Source Wire
  | Or Source Source Wire
  | LeftShift Source Signal Wire
  | RightShift Source Signal Wire
  | Not Source Wire

parseSource :: String -> Source
parseSource source = case Int.fromString source of
  Just signal -> SignalSource $ wrap signal
  Nothing -> WireSource $ wrap source

parseInstruction :: String -> Either String Instruction
parseInstruction value = case map trim $ split (Pattern "->") value of
  [ source, destination ] -> Right $ Input (parseSource source) $ wrap destination
  [ a, "AND", b, destination ] -> Right $ And (parseSource a) (parseSource b) $ wrap destination
  [ a, "OR", b, destination ] -> Right $ Or (parseSource a) (parseSource b) $ wrap destination
  [ source, "LSHIFT", n, destination ] -> case Int.fromString n of
    Just n' -> Right $ LeftShift (parseSource source) (wrap n') $ wrap destination
    Nothing -> Left $ "Failed to read LSHIFT value: " <> n
  [ source, "RSHIFT", n, destination ] -> case Int.fromString n of
    Just n' -> Right $ RightShift (parseSource source) (wrap n') $ wrap destination
    Nothing -> Left $ "Failed to read RSHIFT value: " <> n
  [ "NOT", source, destination ] -> Right $ Not (parseSource source) $ wrap destination
  _ -> Left $ "Failed to parse " <> show value <> " as an instruction."

type Circuit = Map Wire Signal

runInstruction :: Instruction -> Circuit -> Circuit
runInstruction instruction circuit =
  case instruction of
    Input source destination ->
      let
        signal = getSignal source
      in
        circuit # Map.insert destination signal
    And sourceA sourceB destination ->
      let
        signalA = getSignal sourceA
        signalB = getSignal sourceB
      in
        circuit # Map.insert destination (and signalA signalB)
    Or sourceA sourceB destination ->
      let
        signalA = getSignal sourceA
        signalB = getSignal sourceB
      in
        circuit # Map.insert destination (or signalA signalB)
    LeftShift source n destination ->
      let
        signal = getSignal source
      in
        circuit # Map.insert destination (lshift signal n)
    RightShift source n destination ->
      let
        signal = getSignal source
      in
        circuit # Map.insert destination (rshift signal n)
    Not source destination ->
      let
        signal = getSignal source
      in
        circuit # Map.insert destination (complement signal)
  where
    getSignal (WireSource wire) = maybe (wrap 0) identity $ circuit # Map.lookup wire
    getSignal (SignalSource signal) = signal

runCircuit :: Array Instruction -> Circuit
runCircuit = runCircuit' Map.empty
  where
    runCircuit' circuit instructions =
      case uncons instructions of
        Just { head: instruction, tail: rest } ->
          runCircuit' (runInstruction instruction circuit) rest
        Nothing -> circuit

partOne :: String -> Either String Int
partOne input = do
  instructions <-
    input
      # lines
      # traverse parseInstruction
  let wire = Wire "a"
  runCircuit instructions
    # Map.lookup wire
    # note ("Failed to get value for wire " <> (show $ unwrap wire))
    # map unwrap

partTwo :: String -> Either String Int
partTwo input = Left "Part Two not implemented."

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
