module Game where

import Data.Maybe

type Height = Int

type Width = Int

type XIndex = Int

type YIndex = Int

data GameStatus = Won | Lost | Running deriving (Eq, Show)

data Player = Self | Opponent deriving (Eq, Show)

data Chip = Empty | Chip Player deriving (Eq, Show)

type Column = [Chip]

data Grid = Grid Height [Column] deriving (Eq, Show)

type RowCount = [Int]

type RowCountGrid = [[RowCount]]

defaultWidth = 7

defaultHeight = 6

defaultGrid :: Grid
defaultGrid = genEmptyGrid defaultWidth defaultHeight

genEmptyGrid :: Width -> Height -> Grid
genEmptyGrid width height = Grid height (replicate width ([] :: Column))

insertChip :: Grid -> XIndex -> Player -> Maybe Grid
insertChip grid@(Grid height cols) col player
  | col < 0 = Nothing
  | col >= length cols = Nothing
  | length (cols !! col) >= height = Nothing
  | otherwise = Just (Grid height (take col cols ++ ((Chip player : (cols !! col)) : drop (col + 1) cols)))

getChipAt :: Grid -> XIndex -> YIndex -> Maybe Chip
getChipAt grid@(Grid height cols) x y
  | x < 0 = Nothing
  | x >= width = Nothing
  | y < 0 = Nothing
  | y >= height = Nothing
  | y >= length (cols !! x) = Just Empty
  | otherwise = Just (col !! ((length col - 1) - y))
  where
    col = cols !! x
    width = length cols

rowCount :: Grid -> RowCountGrid
rowCount grid@(Grid height cols) =
  [[[(rowCount' x y a b 1) - 1 | a <- [-1 .. 1], b <- [-1 .. 1]] | y <- [0 .. height - 1]] | x <- [0 .. length cols - 1]]
  where
    rowCount' :: XIndex -> YIndex -> Int -> Int -> Int -> Int
    rowCount' x y a b n
      | isNothing chip = n
      | isNothing neighbor = n
      | a == 0 && b == 0 = n
      | neighbor /= chip = n
      | otherwise = rowCount' (x + a) (y + b) a b (n + 1)
      where
        chip = getChipAt grid x y
        neighbor = getChipAt grid (x + a) (y + b)

checkGameStatus :: Grid -> GameStatus
checkGameStatus grid@(Grid height cols)
  | null results = Running
  | otherwise = let firstResult : _ = results in firstResult
  where
    results = [checkGameStatus' x y | x <- [0 .. length cols - 1], y <- [0 .. (height - 1)], not (null (relevantRows x y)) && getChipAt grid x y /= Just Empty && isJust (getChipAt grid x y)]

    relevantRows x y = filter (>= 3) (rowCount grid !! x !! y)

    checkGameStatus' x y
      | getChipAt grid x y == Just (Chip Self) = Won
      | getChipAt grid x y == Just (Chip Opponent) = Lost