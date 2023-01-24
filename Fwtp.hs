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

defaultHostFwtp :: String
defaultHostFwtp = "127.0.0.1"

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

data FwtpConnection = FwtpConnection Version Socket

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

unpackPacket :: Version -> ByteString -> FwtpPacket
unpackPacket 1 raw
  | packetType == FwtpHandshakeInit = let versionList = map read (getFieldsDelimitedBy defaultListDelimiter (getPacketField 1)) in FwtpHandshakeInitPacket versionList
  | packetType == FwtpHandshakeAck = FwtpHandshakeAckPacket (read $ getPacketField 1)
  | packetType == FwtpError = FwtpErrorPacket (read $ getPacketField 1) (getPacketField 2)
  | packetType == FwtpTurn = FwtpTurnPacket (read $ getPacketField 1)
  | otherwise = FwtpInvaildPacket (Just rawString)
  where
    rawString = unpack $ decodeUtf8 raw
    getPacketField = getFieldDelimitedBy defaultPacketFieldDelimiter rawString
    Just packetType = fwtpPacketType $ read $ getPacketField 0

receiveNext :: FwtpConnection -> IO FwtpPacket
receiveNext conn@(FwtpConnection 1 sock) =
  do
    raw <- recv sock 1024
    let packet = unpackPacket 1 raw
    return packet

receiveUntilNext :: FwtpConnection -> FtwpPacketType -> IO [FwtpPacket]
receiveUntilNext conn@(FwtpConnection 1 sock) packetType = receiveUntilNext' ([] :: [FwtpPacket])
  where
    receiveUntilNext' :: [FwtpPacket] -> IO [FwtpPacket]
    receiveUntilNext' packets =
      do
        packet <- receiveNext conn

        if fwtpPacketOfType packet packetType
          then return (packet : packets)
          else receiveUntilNext' (packet : packets)

getOpponentTurn :: FwtpConnection -> IO (Int, [FwtpPacket])
getOpponentTurn conn =
  do
    turnPacket@(FwtpTurnPacket x) : packets <- receiveUntilNext conn FwtpTurn
    return (x, packets)

sendTurn :: FwtpConnection -> Int -> IO ()
sendTurn conn@(FwtpConnection 1 sock) x =
  do
    _ <- send sock (encodeUtf8 (pack (show (fwtpPacketCode FwtpTurn) ++ defaultPacketFieldDelimiter : show x ++ "\n")))
    return ()

incomingHandshake :: Socket -> IO (Maybe Version)
incomingHandshake sock =
  do
    res@(FwtpHandshakeInitPacket verList) : packets <- receiveUntilNext (FwtpConnection fwtpVersion sock) FwtpHandshakeInit

    let matchingVersion
          | fwtpVersion `elem` verList = Just fwtpVersion
          | otherwise = Nothing

    _ <- send sock (encodeUtf8 (pack (show (fwtpPacketCode FwtpHandshakeAck) ++ defaultPacketFieldDelimiter : show fwtpVersion ++ "\n")))

    return matchingVersion

outgoingHandshake :: Socket -> IO (Maybe Version)
outgoingHandshake sock =
  do
    _ <- send sock (encodeUtf8 (pack (show (fwtpPacketCode FwtpHandshakeInit) ++ defaultPacketFieldDelimiter : show fwtpVersion ++ "\n")))

    res@(FwtpHandshakeAckPacket matchingVer) : packets <- receiveUntilNext (FwtpConnection fwtpVersion sock) FwtpHandshakeAck

    return (Just matchingVer)

serveFwtp :: IO (Maybe FwtpConnection)
serveFwtp =
  withSocketsDo $ do
    let hints = defaultHints {addrFlags = [AI_NUMERICHOST], addrSocketType = Stream}
    addr : _ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just (show defaultPortFwtp))
    listenSock <- openSocket addr

    setSocketOption listenSock ReuseAddr 1

    Network.Socket.bind listenSock (addrAddress addr)
    Network.Socket.listen listenSock 1

    (sock, addr) <- accept listenSock

    matchingVersion <- incomingHandshake sock

    case matchingVersion of
      Just ver -> return $ Just $ FwtpConnection ver sock
      Nothing -> return Nothing

connectFwtp :: String -> PortNumber -> IO (Maybe FwtpConnection)
connectFwtp host port =
  withSocketsDo $ do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock (SockAddrInet port (ip4StringToHostAddress host))

    matchingVersion <- outgoingHandshake sock

    case matchingVersion of
      Just ver -> return $ Just $ FwtpConnection ver sock
      Nothing -> return Nothing

-- NOTICE: Just for testing
debugServe :: IO ()
debugServe =
  do
    Just conn <- serveFwtp
    debugServe' conn
  where
    debugServe' :: FwtpConnection -> IO ()
    debugServe' conn@(FwtpConnection 1 sock) =
      do
        raw <- recv sock 1024

        putStr "Incoming: "
        B.putStr raw

        putStr "Unpacked: "
        print $ unpackPacket 1 raw

        debugServe' conn