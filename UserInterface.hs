module UserInterface where

import Game
import System.Exit
import Text.Read (readMaybe)

welcomeBanner =
  colorEmphasis ++
  "             - 𝐂𝐎𝐍𝐍𝐄𝐂𝐓 𝐅𝐎𝐔𝐑 -        \n\
  \        powered by FWTP and Haskell\n\
  \© Antricks 2023 https://github.com/Antricks\n\n"
  ++ colorReset

methodMenu =
  "Choose your method of playing:\n\
  \  [1] Local game\n\
  \  [2] Connect to other FWTP instance\n\
  \  [3] Host a game\n\n\
  \> "

returnToHome = "Press [Enter] to return to the main menu..."

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
clearScreen = putStr "\ESC[H\ESC[2J"

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
      Just a ->
        if a >= 1 && a <= width
          then return (a - 1)
          else getTurn width

showGrid :: Grid -> IO ()
showGrid grid@(Grid height cols) =
  do
    putStr colorReset
    print grid -- TODO schöne ausgabe

evalGameStatus :: Grid -> IO Bool
evalGameStatus grid = evalGameStatus' $ checkGameStatus grid
  where
    evalGameStatus' Won = do showVictory; return True
    evalGameStatus' Lost = do showDefeat; return True
    evalGameStatus' Running = return False

showVictory :: IO ()
showVictory =
  do
    putStr colorPositive
    putStrLn msgVictory
    putStr colorReset
    waitForEnter returnToHome

showDefeat :: IO ()
showDefeat =
  do
    putStr colorNegative
    putStrLn msgDefeat
    putStr colorNegative
    waitForEnter returnToHome

waitForEnter :: String -> IO ()
waitForEnter prompt =
  do
    putStr prompt
    _ <- getLine
    return ()
