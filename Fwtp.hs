module Fwtp where

import Data.Binary (Word8)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Game
import Network.Socket
import Network.Socket.ByteString (recv, send)
import UserInterface (getTurn)
import Utils

defaultPortFwtp :: PortNumber
defaultPortFwtp = 4444

fwtpVersion :: Version
fwtpVersion = 1

defaultPacketFieldDelimiter :: Delimiter
defaultPacketFieldDelimiter = '|'

defaultListDelimiter :: Delimiter
defaultListDelimiter = ','

type FwtpErrorCode = Int

type FwtpErrorMsg = String

type Version = Int

type VersionList = [Version]

data FwtpPacket = FwtpHandshakeInitPacket VersionList | FwtpHandshakeAckPacket Version | FwtpErrorPacket FwtpErrorCode FwtpErrorMsg | FwtpTurnPacket XIndex | FwtpInvaildPacket (Maybe String) deriving (Eq, Show)

data FtwpPacketType = FwtpHandshakeInit | FwtpHandshakeAck | FwtpError | FwtpTurn | FwtpInvaild deriving (Eq, Show)

fwtpPacketCode :: FtwpPacketType -> Int
fwtpPacketCode FwtpHandshakeInit = 0
fwtpPacketCode FwtpHandshakeAck = 1
fwtpPacketCode FwtpError = 2
fwtpPacketCode FwtpTurn = 3

fwtpPacketType :: Int -> Maybe FtwpPacketType
fwtpPacketType i
  | i == fwtpPacketCode FwtpHandshakeInit = Just FwtpHandshakeInit
  | i == fwtpPacketCode FwtpHandshakeAck = Just FwtpHandshakeAck
  | i == fwtpPacketCode FwtpError = Just FwtpError
  | i == fwtpPacketCode FwtpTurn = Just FwtpTurn
  | otherwise = Nothing

fwtpPacketOfType :: FwtpPacket -> FtwpPacketType -> Bool
fwtpPacketOfType (FwtpHandshakeInitPacket _) FwtpHandshakeInit = True
fwtpPacketOfType (FwtpHandshakeAckPacket _) FwtpHandshakeAck = True
fwtpPacketOfType (FwtpErrorPacket _ _) FwtpError = True
fwtpPacketOfType (FwtpTurnPacket _) FwtpTurn = True
fwtpPacketOfType (FwtpInvaildPacket _) FwtpInvaild = True
fwtpPacketOfType _ _ = False

unpackPacket :: ByteString -> FwtpPacket
unpackPacket raw
  | packetType == FwtpHandshakeInit = let versionList = map read (getFieldsDelimitedBy defaultListDelimiter (getPacketField 1)) in FwtpHandshakeInitPacket versionList
  | packetType == FwtpHandshakeAck = FwtpHandshakeAckPacket (read $ getPacketField 1)
  | packetType == FwtpError = FwtpErrorPacket (read $ getPacketField 1) (getPacketField 2)
  | packetType == FwtpTurn = FwtpTurnPacket (read $ getPacketField 1)
  | otherwise = FwtpInvaildPacket (Just rawString)
  where
    rawString = unpack $ decodeUtf8 raw
    getPacketField = getFieldDelimitedBy defaultPacketFieldDelimiter rawString
    Just packetType = fwtpPacketType $ read $ getPacketField 0

receiveNext :: Socket -> FtwpPacketType -> IO [FwtpPacket]
receiveNext sock packetType = receiveNext' ([] :: [FwtpPacket])
  where
    receiveNext' :: [FwtpPacket] -> IO [FwtpPacket]
    receiveNext' packets =
      do
        raw <- recv sock 1024
        let packet = unpackPacket raw

        if fwtpPacketOfType packet packetType
          then return (packet : packets)
          else receiveNext' (packet : packets)

getOpponentTurn :: Socket -> IO (Int, [FwtpPacket])
getOpponentTurn sock =
  do
    turnPacket@(FwtpTurnPacket x) : packets <- receiveNext sock FwtpTurn
    return (x, packets)

sendTurn :: Socket -> Int -> IO ()
sendTurn sock x =
  do
    _ <- send sock (encodeUtf8 (pack (show (fwtpPacketCode FwtpTurn) ++ defaultPacketFieldDelimiter : show x ++ "\n")))
    return ()

serveFwtp :: IO Socket
serveFwtp =
  withSocketsDo $ do
    let hints = defaultHints {addrFlags = [AI_NUMERICHOST], addrSocketType = Stream}
    addr : _ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show defaultPortFwtp))
    sock <- openSocket addr

    Network.Socket.bind sock (addrAddress addr)
    Network.Socket.listen sock 1

    (conn, addr) <- accept sock

    return conn

connectFwtp :: String -> PortNumber -> IO Socket
connectFwtp host port =
  withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (SockAddrInet port (ip4StringToHostAddress host))

    return sock

-- NOTICE: Just for testing
debugServe :: IO ()
debugServe =
  do
    sock <- serveFwtp
    debugServe' sock
  where
    debugServe' sock =
      do
        raw <- recv sock 1024

        putStr "Incoming: "
        B.putStr raw

        putStr "Unpacked: "
        print $ unpackPacket raw

        debugServe' sock