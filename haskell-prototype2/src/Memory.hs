module Memory
    ( Memory
    , Byte
    , Addr
    , Size
    , Slice
     -- access/mutation with 'binary' package combinators
    , getMemory
    , putMemory
    -- primitive api
    , newMemory
        -- access/mutation with lazy byte strings
    , readSlice
    , writeSlice
    -- debug utilities
    , dumpSlice
    ) where

import Memory.Base

import Numeric (showHex)
import Data.List (intercalate)

import Foreign.Storable (sizeOf)
import Data.Array.Base (unsafeRead)


dumpSlice :: Memory -> Slice -> IO String
dumpSlice mem (start,end) = format <$> access
    where
    access = _checkRange mem (start,end) $
        sequence [unsafeRead mem (fromIntegral i) | i <- [start .. end]]
    format :: [Byte] -> String
    format bytes =
        let hexCodes = concatMap toHex bytes
            padLength = start `rem` 16
            startAddr = start - padLength
            addresses = [startAddr, startAddr + 16 ..]

            addrStrs = (++ ": ") . showAddr <$> addresses
            padding = replicate (fromIntegral $ 2 * padLength) ' '
            splitUp = splitEach 8 $ splitEach 4 (padding ++ hexCodes)
            lines = intercalate " " <$> splitUp
            rejoined = intercalate "\n" $ zipWith (++) addrStrs lines
        in rejoined
    toHex byte | byte < 16 = '0' : showHex byte ""
               | otherwise = showHex byte ""
    splitEach n [] = []
    splitEach n str = take n str : splitEach n (drop n str)
    showAddr addr =
        let base = showHex addr ""
            padding = replicate (2 * sizeOf (undefined :: Addr) - length base) '0'
        in "0x" ++ padding ++ base
