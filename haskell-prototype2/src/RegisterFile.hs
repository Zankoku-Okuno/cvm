module RegisterFile
    ( RegisterFile
    , getIP, putIP
    , ProcessorStatus(..)
    , setWindow, moveWindow, getReg, putReg
    ) where

import Data.Word
import Data.IORef

import Foreign.Storable (Storable, sizeOf)
import Data.Binary.Get
import Data.Binary.Put

import RegisterStack (RegisterStack, newRegisterStack)
import qualified RegisterStack as R

data RegisterFile = RF
    { _psr :: !(IORef ProcessorStatus)
    , _gprs :: !RegisterStack
    }

data ProcessorStatus
    = WaitForInterrupt
    | ExecuteMode
        { mode :: !String -- FIXME right now, this names a decoder, but it should probably store a decoder, or at least a numerical index for the mode
        , interruptLevel :: !Word8
        }

newRegisterFile :: Int -> IO RegisterFile
newRegisterFile stackSize = do
    _psr <- newIORef WaitForInterrupt
    _gprs <- newRegisterStack (sizeOf (undefined :: Word), stackSize, 256, 16)
    pure RF{..}

{- No IP (instruction pointer) Register:
I've decided that the IP is always GPR0 to minimize addressing modes
(IP-relative is necessary for PIC after all).
This module provides accessor/mutator functions to use the IP by name.
-}

{-# INLINE getIP #-}
getIP :: RegisterFile -> IO Word
getIP RF{..} = R.getReg getWordhost _gprs 0

{-# INLINE putIP #-}
putIP :: RegisterFile -> Word -> IO ()
putIP RF{..} = R.putReg putWordhost _gprs 0


{- No FLAGS register:
The DEC Alpha didn't have flags, but that's because the flags register could be a bottleneck
for multiple instruction issue. I'm not doing multiple instruction issue, so that's irrelevant.
However, hardware can easily set flags all the time without time penalty (only power cost),
which is not something I can do in a VM. Perhaps it's best to not have a flags register.

For branching on lt,eq,gt,&c, I can simply select a register to test.
Carry is not important most of the time, but when I need it, I can use *-with-carry instructions
that place their carry somewhere else (either selected, or just the next location after the destination).
-}

{-# INLINE setWindow #-}
setWindow :: RegisterFile -> Int -> IO ()
setWindow RF{..} = R.setWindow _gprs

{-# INLINE moveWindow #-}
moveWindow :: RegisterFile -> Int -> IO ()
moveWindow RF{..} = R.moveWindow _gprs

{-# INLINE getReg #-}
getReg :: forall a. Storable a => Get a -> RegisterFile -> Int -> IO a
getReg get RF{..} = R.getReg get _gprs

{-# INLINE putReg #-}
putReg :: forall a. Storable a => (a -> Put) -> RegisterFile -> Int -> a -> IO ()
putReg put RF{..} = R.putReg put _gprs
