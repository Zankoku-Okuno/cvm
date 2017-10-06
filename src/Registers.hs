module Registers
    ( RegisterFile
    , readReg, writeReg
    , newRegisterFile
    , sp_reg, fp_reg
    ) where

import Data.IORef
import Data.Word
import qualified Data.ByteString.Lazy as LBS


newtype RegisterFile = R (IORef [LBS.ByteString]) -- FIXME use a RAM data structure

newRegisterFile :: IO RegisterFile
newRegisterFile = R <$> newIORef (replicate 0x100 $ LBS.replicate 8 0)

-- load register as bytes
readReg :: RegisterFile -> Word8 -> IO LBS.ByteString
readReg _ 0 = pure $ LBS.replicate 8 0 -- FIXME replicate just enough for the largest operand scale
readReg (R cell) i = do
    regs <- readIORef cell
    pure $ regs !! fromIntegral i

-- store bytes to register
writeReg :: RegisterFile -> Word8 -> LBS.ByteString -> IO ()
writeReg regs 0 _ = pure ()
writeReg (R cell) i val = do
    regs <- readIORef cell
    let (pre, post) = splitAt (fromIntegral i) regs
    writeIORef cell $ pre ++ val : drop 1 post

sp_reg :: Word8
sp_reg = 0xff
fp_reg :: Word8
fp_reg = 0xfe