module UserInterface where

import Game
import System.Exit
import Text.Read (readMaybe)

colorGridBorder = "\ESC[36m"

colorOwn = "\ESC[94m"

colorOpp = "\ESC[91m"

colorEmphasis = "\ESC[33m"

colorError = "\ESC[91m"

colorPositive = "\ESC[92m"

colorNegative = "\ESC[91m"

colorReset = "\ESC[0m"

symbolChip = "⬤"

symbolHint = "⚠"

symbolWarning = "⚠"

gridEdgeTopLeft = "┏"

gridEdgeTopRight = "┓"

gridEdgeBotLeft = "┗"

gridEdgeBotRight = "┛"

gridHorizontal = "━"

gridVertical = "┃"

gridCross = "╋"

gridCrossTop = "┳"

gridCrossBot = "┻"

msgVictory = "𝐂𝐎𝐍𝐆𝐑𝐀𝐓𝐒 - 𝐘𝐎𝐔 𝐖𝐎𝐍!                   "

msgDefeat = "𝐘𝐎𝐔 𝐋𝐎𝐒𝐓. 𝐁𝐄𝐓𝐓𝐄𝐑 𝐋𝐔𝐂𝐊 𝐍𝐄𝐗𝐓 𝐓𝐈𝐌𝐄!                       "

clearScreen :: IO ()
clearScreen = return () -- putStr "\ESC[H\ESC[2J"

indicatePlayer :: Player -> IO ()
indicatePlayer Self = putStr (colorOwn ++ "[" ++ symbolChip ++ "] ")
indicatePlayer Opponent = putStr (colorOpp ++ "[" ++ symbolChip ++ "] ")

getTurn :: Width -> IO Int
getTurn width =
  do
    putStr (colorEmphasis ++ "Enter a number from 1 - " ++ show width ++ "> " ++ colorReset)
    input <- getLine
    putStr colorReset

    let turn = readMaybe input :: Maybe Int
    case turn of
      Nothing -> getTurn width
      Just a -> return (a - 1)

showGrid :: Grid -> IO ()
showGrid grid@(Grid height cols) =
  do
    putStr colorReset
    print grid -- TODO schöne ausgabe

evalGameStatus :: Grid -> IO ()
evalGameStatus grid = evalGameStatus' $ checkGameStatus grid
  where
    evalGameStatus' Won = showVictory
    evalGameStatus' Lost = showDefeat
    evalGameStatus' Running = return ()

showVictory :: IO ()
showVictory =
  do
    putStr colorPositive
    putStrLn msgVictory
    putStr colorReset
    exitSuccess

showDefeat :: IO ()
showDefeat =
  do
    putStr colorNegative
    putStrLn msgDefeat
    putStr colorNegative
    exitSuccess