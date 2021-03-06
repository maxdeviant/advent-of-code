module Lib
  ( partOne
  , partTwo
  ) where

import Data.List (partition)
import Data.List.Split (splitOn)

data Orbiters =
  Orbiter String [Orbiters]
  deriving (Show)

data UniversalOrbitMap =
  CenterOfMass [Orbiters]
  deriving (Show)

untilJust :: (a -> Maybe a) -> [a] -> Maybe [a]
untilJust _ [] = Nothing
untilJust f (x:xs) =
  case f x of
    Just g -> Just $ g : xs
    Nothing ->
      case untilJust f xs of
        Just gs -> Just $ x : gs
        Nothing -> Nothing

tryInsertOrbit :: (String, String) -> Orbiters -> Maybe Orbiters
tryInsertOrbit (orbiting, orbiter) (Orbiter name orbiters)
  | name == orbiting = Just $ Orbiter name $ (Orbiter orbiter []) : orbiters
tryInsertOrbit orbit (Orbiter name orbiters) = do
  orbiters' <- untilJust (tryInsertOrbit orbit) orbiters
  Just $ Orbiter name orbiters'

buildUniversalOrbitMap :: String -> UniversalOrbitMap
buildUniversalOrbitMap input = buildUniversalOrbitMap' centerOfMass orbits
  where
    orbitData =
      map (\(orbiting:orbiter:[]) -> (orbiting, orbiter)) $
      map (splitOn ")") $ lines input
    (orbitsAroundCom, orbits) =
      partition (\(orbiting, _) -> orbiting == "COM") orbitData
    centerOfMass =
      CenterOfMass $ map (\(_, orbiter) -> Orbiter orbiter []) orbitsAroundCom
    buildUniversalOrbitMap' orbitMap [] = orbitMap
    buildUniversalOrbitMap' (CenterOfMass orbiters) (orbit:orbits) =
      case untilJust (tryInsertOrbit orbit) orbiters of
        Just orbiters' ->
          buildUniversalOrbitMap' (CenterOfMass orbiters') orbits
        Nothing ->
          buildUniversalOrbitMap' (CenterOfMass orbiters) (orbits ++ [orbit])

countDirectAndIndirectOrbits :: UniversalOrbitMap -> Int
countDirectAndIndirectOrbits (CenterOfMass orbiters) =
  sum $ map (countDirectAndIndirectOrbits' 0) orbiters
  where
    countDirectAndIndirectOrbits' acc (Orbiter _ []) = acc + 1
    countDirectAndIndirectOrbits' acc (Orbiter _ orbiters) =
      acc + 1 + (sum $ map (countDirectAndIndirectOrbits' (acc + 1)) orbiters)

partOne :: String -> Int
partOne = countDirectAndIndirectOrbits . buildUniversalOrbitMap

transfersBetweenOrbits :: String -> String -> UniversalOrbitMap -> Int
transfersBetweenOrbits origin destination orbitMap = 0

partTwo :: String -> Int
partTwo = transfersBetweenOrbits "YOU" "SAN" . buildUniversalOrbitMap
