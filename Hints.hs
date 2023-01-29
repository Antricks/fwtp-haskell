module Hints where

import Data.Maybe (isNothing)
import Game

data Hint = CouldEndGame Player | NoHint | Occupied deriving (Show, Eq)

type HintGrid = [[Hint]]

getHints :: Grid -> HintGrid
getHints grid@(Grid height cols) = [[hint x y | y <- [0 .. height - 1]] | x <- [0 .. width - 1]]
  where
    width = length cols

    hint :: XIndex -> YIndex -> Hint
    hint x y
      | getChipAt grid x y /= Just Empty = Occupied
      | isNothing $ getChipAt grid x y = NoHint
      | decideCouldEndGame Opponent = CouldEndGame Opponent
      | decideCouldEndGame Self = CouldEndGame Self
      | otherwise = NoHint
      where
        decideCouldEndGame :: Player -> Bool
        decideCouldEndGame player = or [decideCouldEndGame' a b | a <- [-1 .. 1], b <- [-1 .. 1]]
          where
            decideCouldEndGame' :: Int -> Int -> Bool
            decideCouldEndGame' a b =
              neighbourIsPlayers
                && ( let neighbourCount = rowCount grid !! (x + a) !! (y + b) !! (3 * (1 + a) + (1 + b))
                      in (neighbourCount == 2)
                      || ( let invNeighborCount = rowCount grid !! (x - a) !! (y - b) !! (3 * (1 - a) + (1 - b))
                            in invNeighborIsPlayers && neighbourIsPlayers && neighbourCount + invNeighborCount >= 1
                         )
                   )
              where
                neighbourIsPlayers = getChipAt grid (x + a) (y + b) == Just (Chip player)
                invNeighborIsPlayers = getChipAt grid (x - a) (y - b) == Just (Chip player)
