module Day1 (partOne, partTwo) where

data Direction = Up | Down deriving (Show)

parseDirection :: Char -> Char -> Char -> Either String Direction
parseDirection upToken _ token | upToken == token = Right Up
parseDirection _ downToken token | downToken == token = Right Down
parseDirection upToken downToken token =
  Left $
    "Failed to parse "
      ++ show token
      ++ " as a valid direction. Expected "
      ++ show upToken
      ++ " for "
      ++ show Up
      ++ " or "
      ++ show downToken
      ++ " for "
      ++ show Down
      ++ "."

parseInstructions :: String -> Either String [Direction]
parseInstructions = traverse $ parseDirection '(' ')'

move :: Direction -> Int
move Up = 1
move Down = -1

positionOfFirstBasementDirection :: [Direction] -> Either String Int
positionOfFirstBasementDirection = go 0 0
  where
    go santasPosition cursor _ | santasPosition < 0 = Right cursor
    go santasPosition cursor directions =
      case directions of
        (direction : tail) -> go (santasPosition + move direction) (cursor + 1) tail
        [] -> Left "Santa never entered the basement!"

partOne :: String -> Either String Int
partOne input = do
  instructions <- parseInstructions input
  pure $ sum . map move $ instructions

partTwo :: String -> Either String Int
partTwo input = do
  instructions <- parseInstructions input
  positionOfFirstBasementDirection instructions
