module Main where

import Control.Monad (when)
import Fwtp
import Game
import Network.Socket (PortNumber)
import Text.Read (readMaybe)
import UserInterface
import Utils (execAll)

handlePacket :: FwtpConnection -> FwtpPacket -> IO () -- NOTICE: This is non-exhaustive. Should I add anything except errors here though?
handlePacket conn@(FwtpConnection 1 sock) (FwtpErrorPacket code msg) = showError code msg

gameLoopLocal :: Grid -> IO ()
gameLoopLocal grid@(Grid height cols) =
  do
    clearScreen
    showGrid grid
    gameEnded <- evalGameStatus grid
    when gameEnded main

    indicatePlayer Self
    ownTurn <- getTurn width
    let Just ownResult = insertChip grid ownTurn Self -- TODO: handle Nothing
    clearScreen
    showGrid ownResult
    gameEnded <- evalGameStatus ownResult
    when gameEnded main

    indicatePlayer Opponent
    oppTurn <- getTurn width
    let Just oppResult = insertChip ownResult oppTurn Opponent -- TODO: handle Nothing
    gameLoopLocal oppResult
  where
    width = length cols

gameLoopNetwork :: FwtpConnection -> Grid -> IO ()
gameLoopNetwork conn@(FwtpConnection 1 sock) grid@(Grid height cols) =
  do
    clearScreen
    showGrid grid
    gameEnded <- evalGameStatus grid
    when gameEnded main

    ownTurn <- getTurn width
    sendTurn conn ownTurn
    let Just ownResult = insertChip grid ownTurn Self -- TODO: handle Nothing
    clearScreen
    showGrid ownResult
    gameEnded <- evalGameStatus ownResult
    when gameEnded main

    (oppTurn, otherPackets) <- getOpponentTurn conn
    let actions = map (handlePacket conn) otherPackets

    execAll actions -- TODO: FIXME this evaluates only after opponent submitted a turn. Until then, errors are not displayed.
    let Just oppResult = insertChip ownResult oppTurn Opponent -- TODO: handle Nothing
    gameLoopNetwork conn oppResult
  where
    width = length cols

mainServe :: IO ()
mainServe =
  do
    putStrLn (colorEmphasis ++ "\n[INFO] Waiting for connection..." ++ colorReset)
    conn <- serveFwtp

    case conn of
      Just c -> gameLoopNetwork c defaultGrid
      Nothing ->
        do
          showError 101 "No matching FWTP version found. The clients are incompatible."
          main

mainConnect :: String -> Maybe PortNumber -> IO ()
mainConnect "" port = mainConnect "127.0.0.1" port
mainConnect host Nothing = mainConnect host (Just defaultPortFwtp)
mainConnect host (Just port) =
  do
    conn <- connectFwtp host port

    case conn of
      Just conn ->
        do
          let grid = defaultGrid

          clearScreen
          showGrid grid
          evalGameStatus grid

          (oppTurn, otherPackets) <- getOpponentTurn conn
          print otherPackets

          let Just oppResult = insertChip grid oppTurn Opponent -- TODO: handle Nothing
          gameLoopNetwork conn oppResult
      Nothing ->
        do
          showError 101 "No matching FWTP version found. The clients are incompatible."
          main

mainLocal :: IO ()
mainLocal = gameLoopLocal defaultGrid

main :: IO ()
main =
  do
    clearScreen
    putStrLn welcomeBanner

    putStr methodMenu
    choiceRaw <- getLine

    let choice = readMaybe choiceRaw :: Maybe Int

    case choice of
      Just 1 -> mainLocal
      Just 2 ->
        do
          -- TODO maybe make this a bit more resistent to malformed input
          putStr ("Please enter the IPv4 address of the host you want to connect to [leave empty for " ++ defaultHostFwtp ++ "]: ")
          host <- getLine

          putStr ("Please enter the port you want to use [leave empty for " ++ show defaultPortFwtp ++ "]: ")
          portRaw <- getLine

          let port = readMaybe portRaw :: Maybe PortNumber

          mainConnect host port -- port==Nothing is handled inside of mainConnect.
      Just 3 -> mainServe
      Just _ -> main
      Nothing -> main
