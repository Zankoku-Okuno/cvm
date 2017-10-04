module Memory
    ( Mem
    , MemConfig(..)
    , newMem
    , readBytes
    , writeBytes
    , addresses
    ) where

import Control.Monad
import Data.Array.MArray
import Data.Array.IO
import Data.Word
import qualified Data.ByteString.Lazy as LBS


data Mem = Mem
    { heap :: IOUArray Word Word8
    , stacks :: IOUArray Word Word8
    , heapRange_ :: (Word, Word)
    , stacksRange_ :: (Word, Word)
    }

addresses :: Mem -> ((Word, Word), (Word, Word))
addresses Mem{..} = (heapRange_, stacksRange_)

data MemConfig = MemConfig
    { rootStackSize :: Word
    , threadStackSize :: Word
    , maxThreads :: Word
    , heapSize :: Word
    }

newMem :: MemConfig -> IO Mem
newMem MemConfig{..} = do
    heap <- newArray_ (0, heapSize - 1)
    heapRange_ <- getBounds heap
    let stacksSize = rootStackSize + (maxThreads-1)*threadStackSize
    stacks <- newArray_ (maxBound - stacksSize + 1, maxBound)
    stacksRange_ <- getBounds stacks
    pure Mem{..}

region :: Mem -> Word -> IOUArray Word Word8
Mem{..} `region` addr = if inRange heapRange_ addr then heap else stacks -- FIXME also range check stacks, else create a better error message

readByte :: Mem -> Word -> IO Word8
readByte mem addr = readArray (mem `region` addr) addr

writeByte :: Mem -> Word -> Word8 -> IO ()
writeByte mem addr v' = writeArray (mem `region` addr) addr v'

enumerate :: LBS.ByteString -> [(Word, Word8)]
enumerate bytes = zip [0 .. fromIntegral (LBS.length bytes) - 1] (LBS.unpack bytes)

readBytes :: Mem -> Word -> Word -> IO LBS.ByteString
readBytes mem addr len = LBS.pack <$> forM [addr..addr+len-1] (readByte mem)
writeBytes :: Mem -> Word -> LBS.ByteString -> IO ()
writeBytes mem addr v = do
    forM_ (enumerate v) $ \(i, byte) -> -- FIXME there must be a more idomatic way
        writeByte mem (addr + i) byte
