module Main where

import Fwtp
import Game
import Network.Socket
import UserInterface

handlePacket :: Socket -> FwtpPacket -> IO ()
handlePacket sock (FwtpHandshakeInitPacket verList) = undefined -- TODO (oder vielleicht auch einfach nicht?)
handlePacket sock (FwtpHandshakeAckPacket ver) = undefined -- TODO (oder vielleicht auch einfach nicht?)
handlePacket sock (FwtpErrorPacket code msg) = undefined -- TODO: Fehler Anzeigen

gameLoopLocal :: Grid -> IO ()
gameLoopLocal grid@(Grid height cols) =
  do
    clearScreen
    showGrid grid
    evalGameStatus grid

    indicatePlayer Self
    ownTurn <- getTurn width
    let Just ownResult = insertChip grid ownTurn Self -- TODO: Nothing abfangen
    clearScreen
    showGrid ownResult
    evalGameStatus ownResult

    indicatePlayer Opponent
    oppTurn <- getTurn width
    let Just oppResult = insertChip ownResult oppTurn Opponent -- TODO: Nothing abfangen
    gameLoopLocal oppResult
  where
    width = length cols

gameLoopNetwork :: Socket -> Grid -> IO ()
gameLoopNetwork sock grid@(Grid height cols) =
  do
    clearScreen
    showGrid grid
    evalGameStatus grid

    ownTurn <- getTurn width
    sendTurn sock ownTurn
    let Just ownResult = insertChip grid ownTurn Self -- TODO: Nothing abfangen
    clearScreen
    showGrid ownResult
    evalGameStatus ownResult

    (oppTurn, otherPackets) <- getOpponentTurn sock
    print otherPackets

    let Just oppResult = insertChip ownResult oppTurn Opponent -- TODO: Nothing abfangen
    gameLoopNetwork sock oppResult
  where
    width = length cols

mainServe :: IO ()
mainServe =
  do
    sock <- serveFwtp
    gameLoopNetwork sock defaultGrid

mainConnect :: String -> Maybe PortNumber -> IO ()
mainConnect host Nothing = mainConnect host (Just defaultPortFwtp)
mainConnect host (Just port) =
  do
    sock <- connectFwtp host port

    let grid = defaultGrid

    clearScreen
    showGrid grid
    evalGameStatus grid

    (oppTurn, otherPackets) <- getOpponentTurn sock
    print otherPackets

    let Just oppResult = insertChip grid oppTurn Opponent -- TODO: Nothing abfangen
    gameLoopNetwork sock oppResult

mainLocal :: IO ()
mainLocal = gameLoopLocal defaultGrid

main :: IO ()
main = mainServe -- TODO: nutzerfreundliche Entscheidung was wir jetzt eigentlich machen wollen