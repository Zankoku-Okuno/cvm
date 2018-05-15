module Main where

import Numeric (showHex)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Types
import Metadata
import Memory
import RegisterStack
import RegisterStack.Internal (_regs)

main :: IO ()
main = do
    putStrLn $ if bigEndian hostMetadata then "Big Endian" else "Little Endian"
    putStrLn "------------------------------------"
    mem <- newMemory 0x21
    writeSlice mem (0, 0x21) $ LBS.pack [
        0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,
        0,0,0,0,0xde,0xad,0xbe,0xef,0,0,0,0,0xde,0xad,0xbe,0xef,
        0xFF,0xFF
        ]
    putStrLn =<< dumpSlice mem (0x14,14)
    putMemory putWord16host mem 0 0xabcd
    putStrLn . flip showHex "" =<< getMemory getWord16host mem 0
    putStrLn "------------------------------------"
    rf <- newRegisterStack (4, 16, 8, 2)
    sequence_ [putReg putWord8 rf i (10 + fromIntegral i) | i <- [0 .. 7]]
    moveWindow rf 7
    sequence_ [putReg putWord8 rf (i + 2) (0xF0 + fromIntegral i) | i <- [0 .. 5]]
    putStrLn =<< dumpSlice (_regs rf) (0, 16*4)
