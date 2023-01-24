module Main where

import Fwtp
import Game

import UserInterface
import Text.Read (readMaybe)
import Control.Monad (when)
import Network.Socket (PortNumber)

handlePacket :: FwtpConnection -> FwtpPacket -> IO ()
handlePacket conn@(FwtpConnection 1 sock) (FwtpHandshakeInitPacket verList) = undefined -- TODO (oder vielleicht auch einfach nicht?)
handlePacket conn@(FwtpConnection 1 sock) (FwtpHandshakeAckPacket ver) = undefined -- TODO (oder vielleicht auch einfach nicht?)
handlePacket conn@(FwtpConnection 1 sock) (FwtpErrorPacket code msg) = undefined -- TODO: Fehler Anzeigen

gameLoopLocal :: Grid -> IO ()
gameLoopLocal grid@(Grid height cols) =
  do
    clearScreen
    showGrid grid
    gameEnded <- evalGameStatus grid
    when gameEnded main

    indicatePlayer Self
    ownTurn <- getTurn width
    let Just ownResult = insertChip grid ownTurn Self -- TODO: Nothing abfangen
    clearScreen
    showGrid ownResult
    gameEnded <- evalGameStatus ownResult
    when gameEnded main

    indicatePlayer Opponent
    oppTurn <- getTurn width
    let Just oppResult = insertChip ownResult oppTurn Opponent -- TODO: Nothing abfangen
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
    let Just ownResult = insertChip grid ownTurn Self -- TODO: Nothing abfangen
    clearScreen
    showGrid ownResult
    gameEnded <- evalGameStatus ownResult
    when gameEnded main

    (oppTurn, otherPackets) <- getOpponentTurn conn
    print otherPackets

    let Just oppResult = insertChip ownResult oppTurn Opponent -- TODO: Nothing abfangen
    gameLoopNetwork conn oppResult
  where
    width = length cols

mainServe :: IO ()
mainServe =
  do
    Just conn <- serveFwtp --TODO Nothing abfangen
    gameLoopNetwork conn defaultGrid

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

          let Just oppResult = insertChip grid oppTurn Opponent -- TODO: Nothing abfangen
          gameLoopNetwork conn oppResult
      Nothing ->
        return () --TODO Fehlermeldung

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
        do --TODO maybe make this a bit more resistent to malformed input
          putStr ("Please enter the IPv4 address of the host you want to connect to [leave empty for " ++ defaultHostFwtp ++ "]: ")
          host <- getLine

          putStr ("Please enter the port you want to use [leave empty for " ++ show defaultPortFwtp ++ "]: ")
          portRaw <- getLine

          let port = readMaybe portRaw :: Maybe PortNumber

          mainConnect host port -- port==Nothing is handled inside of mainConnect.
      Just 3 -> mainServe
      Just _ -> main
      Nothing -> main
            