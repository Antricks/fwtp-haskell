module Utils where

import Data.Binary (Word8)
import Network.Socket

type Delimiter = Char

getFieldDelimitedBy :: Delimiter -> String -> Int -> String
getFieldDelimitedBy delimiter input index = getFieldDelimitedBy' index input
  where
    getFieldDelimitedBy' :: Int -> String -> String
    getFieldDelimitedBy' 0 a = takeWhile (/= delimiter) a
    getFieldDelimitedBy' n a = getFieldDelimitedBy' (n - 1) (tail (dropWhile (/= delimiter) a))

getFieldsDelimitedBy :: Delimiter -> String -> [String]
getFieldsDelimitedBy delimiter = getFieldsDelimitedBy' []
  where
    getFieldsDelimitedBy' :: [String] -> String -> [String]
    getFieldsDelimitedBy' buf "" = buf
    getFieldsDelimitedBy' buf input = getFieldsDelimitedBy' ((takeWhile (/= delimiter) input) : buf) (tail (dropWhile (/= delimiter) input))

ip4StringToTuple :: String -> (Word8, Word8, Word8, Word8)
ip4StringToTuple addr = (parseAddrPart addr 0, parseAddrPart addr 1, parseAddrPart addr 2, parseAddrPart addr 3)
  where
    parseAddrPart :: String -> Int -> Word8
    parseAddrPart a i = (read $ getFieldDelimitedBy '.' a i) :: Word8

ip4StringToHostAddress :: String -> HostAddress
ip4StringToHostAddress = tupleToHostAddress . ip4StringToTuple
