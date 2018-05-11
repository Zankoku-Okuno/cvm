module Main where

import Numeric (showHex)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Foreign.Storable (sizeOf)
import Data.Binary.Get
import Data.Binary.Put

import Memory

main :: IO ()
main = do
    mem <- newMemory 0x21
    writeSlice mem (0, 0x21) $ LBS.pack [
        0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,0xa0,
        0,0,0,0,0xde,0xad,0xbe,0xef,0,0,0,0,0xde,0xad,0xbe,0xef,
        0xFF,0xFF
        ]
    putStrLn =<< dumpSlice mem (0x14,0x21)
    putMemory putWord16host mem 0 0xabcd
    putStrLn . flip showHex "" =<< getMemory getWord16host mem 0
    putStrLn $ if bigEndian hostMetadata then "Big Endian" else "Little Endian"

data Metadata = Metadata
    { addrSize :: !Int
    , bigEndian :: !Bool
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
