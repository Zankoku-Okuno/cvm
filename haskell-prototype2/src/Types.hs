module Types
    ( Byte, Addr, Size, Slice
    , module Data.Word
    , module Data.IORef
    , Storable, sizeOf
    , module Data.Binary.Get
    , module Data.Binary.Put
    ) where

import Data.Word
import Data.IORef

import Foreign.Storable (Storable, sizeOf)
import Data.Binary.Get
import Data.Binary.Put


type Byte = Word8
type Addr = Int
type Size = Word
type Slice = (Addr, Size)

type CpuId = Word16 -- WARNING: I don't know why you'd need more than 65536 cores, but those are famous last words!

