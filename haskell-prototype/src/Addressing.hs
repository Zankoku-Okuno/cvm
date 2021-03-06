module Addressing where

import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Foreign.Storable (sizeOf)
import Data.Word
import Data.Int

import Machine
import Memory
import Registers


------------------ Operands ------------------

data Scale =
      Byte
    | DblByte
    | QuadByte
    | OctByte
    | AddrSize
scaleFactor :: Integral n => Scale -> n
scaleFactor Byte = 1
scaleFactor DblByte = 2
scaleFactor QuadByte = 4
scaleFactor OctByte = 8
scaleFactor AddrSize = fromIntegral $ sizeOf (undefined :: Word)

maxValue :: Integral n => Scale -> n
maxValue Byte = fromIntegral (maxBound :: Word8)
maxValue DblByte = fromIntegral (maxBound :: Word16)
maxValue QuadByte = fromIntegral (maxBound :: Word32)
maxValue OctByte = fromIntegral (maxBound :: Word64)
maxValue AddrSize = fromIntegral (maxBound :: Word)


data Operand =
      Immediate LBS.ByteString -- imm
    | GPR Word8 -- R[x]
    | Offset Word8 Int -- M[R[x] + offset]
    | Deferred Operand -- M[imm], M[R[x]], M[M[R[x] + offset]]


------ load effective address ------

addrOf :: Thread -> Operand -> IO Word
addrOf thread (Immediate _) = error "immediate operands have no address"
addrOf thread (GPR _) = error "registers have no memory address"
addrOf thread (Offset reg offset) = do
    base <- ldu thread (AddrSize, GPR reg)
    pure $ base + fromIntegral offset
addrOf thread (Deferred ptr) = ldu thread (AddrSize, ptr)


------ load/store bytes ------

ldb :: Thread -> (Scale, Operand) -> IO LBS.ByteString
ldb thread (size, Immediate bs) = pure $
    LBS.take (fromIntegral $ scaleFactor size) bs
ldb thread (size, GPR i) = do
    payload <- readReg (gprs thread) i
    pure $ LBS.take (fromIntegral $ scaleFactor size) payload -- FIXME also ensure the bytestring is long enough
ldb thread (size, op) = do
    addr <- addrOf thread op
    readBytes (mem $ machine thread) addr (scaleFactor size)

stb :: Thread -> (Scale, Operand) -> LBS.ByteString -> IO ()
stb thread (size, GPR i) bytes = do
    let payload = LBS.take (fromIntegral $ scaleFactor size) bytes -- FIXME also ensure the bytestring is long enough
    writeReg (gprs thread) i payload
stb thread (size, dst) bytes = do
    let payload = LBS.take (fromIntegral $ scaleFactor size) bytes -- FIXME also ensure the bytestring is long enough
    addr <- addrOf thread dst
    writeBytes (mem $ machine thread) addr payload


------ load/store unsigned ------

ldu :: Integral a => Thread -> (Scale, Operand) -> IO a
ldu thread src@(scale, _) = do
    bytes <- ldb thread src
    pure $ case scale of
            Byte     -> fromIntegral (decode bytes :: Word8)
            DblByte  -> fromIntegral (decode bytes :: Word16)
            QuadByte -> fromIntegral (decode bytes :: Word32)
            OctByte  -> fromIntegral (decode bytes :: Word64)
            AddrSize -> fromIntegral (decode bytes :: Word)

stu :: Integral a => Thread -> (Scale, Operand) -> a -> IO ()
stu thread dst@(scale, _) i = do
    let bytes = case scale of
            Byte     -> encode (fromIntegral i :: Word8)
            DblByte  -> encode (fromIntegral i :: Word16)
            QuadByte -> encode (fromIntegral i :: Word32)
            OctByte  -> encode (fromIntegral i :: Word64)
            AddrSize -> encode (fromIntegral i :: Word)
    stb thread dst bytes


------ load/store signed ------

lds :: Integral a => Thread -> (Scale, Operand) -> IO a
lds thread src@(scale, _) = do
    bytes <- ldb thread src
    pure $ case scale of
            Byte     -> fromIntegral (decode bytes :: Int8)
            DblByte  -> fromIntegral (decode bytes :: Int16)
            QuadByte -> fromIntegral (decode bytes :: Int32)
            OctByte  -> fromIntegral (decode bytes :: Int64)
            AddrSize -> fromIntegral (decode bytes :: Int)

sts :: Integral a => Thread -> (Scale, Operand) -> a -> IO ()
sts thread dst@(scale, _) i = do
    let bytes = case scale of
            Byte     -> encode (fromIntegral i :: Int8)
            DblByte  -> encode (fromIntegral i :: Int16)
            QuadByte -> encode (fromIntegral i :: Int32)
            OctByte  -> encode (fromIntegral i :: Int64)
            AddrSize -> encode (fromIntegral i :: Int)
    stb thread dst bytes


-- TODO load/store floats


------ breaking bytestrings ------

breakParts :: Scale -> LBS.ByteString -> [LBS.ByteString]
breakParts scale = go
    where
    go (LBS.null -> True) = []
    go (LBS.splitAt len -> (bytes, rest))
        | LBS.length bytes /= len = error "byte string not evenly divisible"
        | otherwise = bytes : go rest
    len = scaleFactor scale

