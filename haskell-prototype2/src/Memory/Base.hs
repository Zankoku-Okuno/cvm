{-|
Data types and algorithms for working with a contiguous block of mutable bytes.

Convenience functions for reading standard data types are provided theough the binary package.
-}
module Memory.Base where

import Data.Word
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Data.Array.MArray
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO

import Foreign.Storable (Storable, sizeOf)
import Data.Binary.Get
import Data.Binary.Put

type Byte = Word8
type Addr = Word
type Size = Word
type Slice = (Addr, Addr) -- ^ start and end, inclusive

type Memory = IOUArray Addr Byte


{-# INLINE readSlice #-}
readSlice :: Memory -> Slice -> IO ByteString
readSlice mem (start,end) = _checkRange mem (start,end) $
    LBS.pack <$> sequence [unsafeRead mem (fromIntegral i) | i <- [start .. end]]

{-# INLINE writeSlice #-}
writeSlice :: Memory -> Slice -> ByteString -> IO ()
writeSlice mem (start,end) new = _checkRange mem (start,end) $
    sequence_ [unsafeWrite mem (fromIntegral i) x | (i, x) <- zip [start .. end] (LBS.unpack new)]


{-# INLINE newMemory #-}
newMemory :: Size -> IO Memory
newMemory size = newArray_ (0, size)

{-# INLINE getMemory #-}
getMemory :: forall a. Storable a => Get a -> Memory -> Addr -> IO a
getMemory get mem addr = do
    let size = fromIntegral $ sizeOf (undefined :: a)
    bytes <- readSlice mem (addr, addr + size)
    pure $ runGet get bytes

{-# INLINE putMemory #-}
putMemory :: Storable a => (a -> Put) -> Memory -> Addr -> a -> IO ()
putMemory put mem addr x = do
    let bytes = runPut $ put x
        size = fromIntegral $ LBS.length bytes
    writeSlice mem (addr, addr + size) bytes


{-# INLINE _checkRange #-}
_checkRange :: Memory -> Slice -> IO a -> IO a
_checkRange mem (start, end) action = do
    _ <- readArray mem start
    _ <- readArray mem end
    action
