module Main where

import Numeric (showHex)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Foreign.Storable (sizeOf)
import Data.Binary.Get
import Data.Binary.Put

import Memory
import RegisterStack

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

data Metadata = Metadata
    { addrSize :: !Int
    , bigEndian :: !Bool
    , bitSizes :: ![(Int, String)]
    }

hostMetadata :: Metadata
hostMetadata = Metadata {..}
    where
    addrSize = sizeOf (undefined :: Addr)
    bigEndian =
        let testData = 1
            written = runPut $ putWord16host testData
            readBack = flip runGet written $ getWord16be
        in readBack == testData
    bitSizes =
        [ (8 , "byte")
        , (16, "wide")
        , (32, "quad")
        , (64, "oct" )
        ]
