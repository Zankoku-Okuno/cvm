module InstructionSet where

import Data.Word
import Data.Bits
import Numeric.Natural
import qualified Data.ByteString.Lazy as LBS

import Addressing
import Machine


passThrough action v = action v >> pure (Just v)


------ moving data ------

mov :: Scale -> Operand -> Operand -> Instr
mov scale src dst = passThrough $ \thread ->
    ldb thread (scale, src) >>= stb thread (scale, dst)

xchg :: Scale -> Operand -> Operand -> Instr
xchg scale a b = passThrough $ \thread -> do
    tmpA <- ldb thread (scale, a)
    tmpB <- ldb thread (scale, b)
    stb thread (scale, a) tmpB
    stb thread (scale, b) tmpA

lea :: Operand -> Operand -> Instr
lea src dst = passThrough $ \thread -> do
    addr <- addrOf thread src
    stu thread (AddrSize, dst) addr


------ jumping ------

jmp :: Operand -> Instr
jmp src@(Immediate _) thread = do -- relative jump
    offset <- lds thread (AddrSize, src)
    let ip' = ip thread + offset
    pure $ Just thread{ip = ip'}
jmp src thread = do -- (computed) absolute jump
    ip' <- lds thread (AddrSize, src)
    pure $ Just thread{ip = ip'}

link :: Operand -> Operand -> Instr
link src dst thread = do
    stu thread (AddrSize, dst) (ip thread)
    jmp src thread

jmp_cond_signed_ :: (Integer -> Integer -> Bool) -> Operand -> Scale -> Operand -> Operand -> Instr
jmp_cond_signed_ cond j scale srcA srcB thread = do
    a <- lds thread (scale, srcA)
    b <- lds thread (scale, srcB)
    if a `cond` b
    then jmp j thread
    else pure $ Just thread
jmp_cond_unsigned_ :: (Integer -> Integer -> Bool) -> Operand -> Scale -> Operand -> Operand -> Instr
jmp_cond_unsigned_ cond j scale srcA srcB thread = do
    a <- ldu thread (scale, srcA)
    b <- ldu thread (scale, srcB)
    if cond a b
    then jmp j thread
    else pure $ Just thread

jmp_eq :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_eq = jmp_cond_unsigned_ (==)
jmp_neq :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_neq = jmp_cond_unsigned_ (/=)

jmp_lt :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_lt = jmp_cond_signed_ (<)
jmp_lte :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_lte = jmp_cond_signed_ (<=)
jmp_gt :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_gt = jmp_cond_signed_ (>)
jmp_gte :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_gte = jmp_cond_signed_ (>=)

jmp_b :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_b = jmp_cond_signed_ (<)
jmp_be :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_be = jmp_cond_signed_ (<=)
jmp_a :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_a = jmp_cond_signed_ (>)
jmp_ae :: Operand -> Scale -> Operand -> Operand -> Instr
jmp_ae = jmp_cond_signed_ (>=)


------  logic ------

land :: Scale -> Operand -> Operand -> Instr
land scale src dst = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    b <- ldu thread (scale, dst)
    let c :: Integer = a .&. b
    stu thread (scale, dst) c

lor :: Scale -> Operand -> Operand -> Instr
lor scale src dst = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    b <- ldu thread (scale, dst)
    let c :: Integer = a .|. b
    stu thread (scale, dst) c

lxor :: Scale -> Operand -> Operand -> Instr
lxor scale src dst = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    b <- ldu thread (scale, dst)
    let c :: Integer = a `xor` b
    stu thread (scale, dst) c

lnot :: Scale -> Operand -> Instr
lnot scale dst = passThrough $ \thread -> do
    a <- ldu thread (scale, dst)
    let c :: Integer = complement a
    stu thread (scale, dst) c


------  shift and swap ------

lshl :: Operand -> Scale -> Operand -> Operand -> Instr
lshl src scale dst carry = passThrough $ \thread -> do
    amount <- ldu thread (Byte, src)
    a <- ldu thread (scale, dst)
    let c :: Natural = shiftL a amount
        cout = shiftR c (8 * scaleFactor scale)
    stu thread (scale, dst) c
    stu thread (scale, carry) cout

lshr :: Operand -> Scale -> Operand -> Operand -> Instr
lshr src scale dst carry = passThrough $ \thread -> do
    amount <- ldu thread (Byte, src)
    a <- ldu thread (scale, dst)
    let c :: Natural = shiftR a amount
        cout = shiftL a (8 * scaleFactor scale - amount)
    stu thread (scale, dst) c
    stu thread (scale, carry) cout

ashl :: Operand -> Scale -> Operand -> Operand -> Instr
ashl = lshl

ashr :: Operand -> Scale -> Operand -> Operand -> Instr
ashr src scale dst carry = passThrough $ \thread -> do
    amount <- ldu thread (Byte, src)
    a <- lds thread (scale, dst)
    let c :: Integer = shiftR a amount
        cout = shiftL a (8 * scaleFactor scale - amount)
    sts thread (scale, dst) c
    sts thread (scale, carry) cout

rot :: Operand -> Scale -> Operand -> Instr
-- positive left, negative right
rot src scale dst = passThrough $ \thread -> do
    amount <- lds thread (Byte, src)
    a <- ldu thread (scale, dst)
    let c :: Natural = h .|. l
        h = shift a amount
        l = shift a (amount - backAmount)
        backAmount = (if amount < 0 then ((-1)*) else id) (8 * scaleFactor scale)
    stu thread (scale, dst) c

swap :: Scale -> Scale -> Operand -> Instr
swap by scale dst = passThrough $ \thread -> do
    a <- ldb thread (scale, dst)
    let c = LBS.concat . reverse $ breakParts by a
    stb thread (scale, dst) c


------  bit twiddle ------

bt :: Operand -> Scale -> Operand -> Operand -> Instr
bt i scale src dst = passThrough $ \thread -> do
    i <- ldu thread (Byte, i)
    a <- ldu thread (scale, src)
    let c :: Word8 = if (a :: Natural) `testBit` i then 1 else 0
    stu thread (Byte, dst) c

bs :: Operand -> Scale -> Operand -> Instr
bs i scale dst = passThrough $ \thread -> do
    i <- ldu thread (Byte, i)
    a <- ldu thread (scale, dst)
    let c :: Natural = a `setBit` i
    stu thread (scale, dst) c

popcnt :: Scale -> Operand -> Operand -> Instr
popcnt scale src dst = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    let c :: Natural = fromIntegral $ case (scale, a :: Natural) of
            (Byte, fromIntegral -> (a :: Word8)) -> popCount a
            (DblByte, fromIntegral -> (a :: Word16)) -> popCount a
            (QuadByte, fromIntegral -> (a :: Word32)) -> popCount a
            (OctByte, fromIntegral -> (a :: Word64)) -> popCount a
            (AddrSize, fromIntegral -> (a :: Word)) -> popCount a
    stu thread (Byte, dst) c

clz :: Scale -> Operand -> Operand -> Instr
clz scale src dst = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    let c :: Natural = fromIntegral $ case (scale, a :: Natural) of
            (Byte, fromIntegral -> (a :: Word8)) -> countLeadingZeros a
            (DblByte, fromIntegral -> (a :: Word16)) -> countLeadingZeros a
            (QuadByte, fromIntegral -> (a :: Word32)) -> countLeadingZeros a
            (OctByte, fromIntegral -> (a :: Word64)) -> countLeadingZeros a
            (AddrSize, fromIntegral -> (a :: Word)) -> countLeadingZeros a
    stu thread (Byte, dst) c

ctz :: Scale -> Operand -> Operand -> Instr
ctz scale src dst = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    let c :: Natural = fromIntegral $ case (scale, a :: Natural) of
            (Byte, fromIntegral -> (a :: Word8)) -> countTrailingZeros a
            (DblByte, fromIntegral -> (a :: Word16)) -> countTrailingZeros a
            (QuadByte, fromIntegral -> (a :: Word32)) -> countTrailingZeros a
            (OctByte, fromIntegral -> (a :: Word64)) -> countTrailingZeros a
            (AddrSize, fromIntegral -> (a :: Word)) -> countTrailingZeros a
    stu thread (Byte, dst) c


------ integer arithmetic ------

neg :: Scale -> Operand -> Operand -> Instr
neg scale dst carry = passThrough $ \thread -> do
    a <- lds thread (scale, dst)
    let a' :: Integer = -a
        cout = if a == -1 then -1 else 0
    sts thread (scale, dst) a'
    sts thread (scale, carry) cout

-- NOTE add/sub generate unsigned carry only; signed overflow is a different matter
add :: Scale -> Operand -> Operand -> Operand -> Instr
add scale src dst carry = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    b <- ldu thread (scale, dst)
    cin <- ldu thread (scale, carry)
    let c :: Natural = a + b + cin
        cout = if maxValue scale < c then 1 else 0
    stu thread (scale, dst) c
    stu thread (scale, carry) cout

sub :: Scale -> Operand -> Operand -> Operand -> Instr
sub scale src dst carry = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    b <- ldu thread (scale, dst)
    cin <- ldu thread (scale, carry)
    let c :: Integer = b - a - cin
        cout = if c < 0 then 1 else 0
    stu thread (scale, dst) c
    stu thread (scale, carry) cout

-- TODO integer operations
-- mul/div and imul/idiv (two outputs each)
-- signed/unsigned compare (boolean not is just cmp_eq R0)
-- TODO BCD/packed BCD?


------ floating-point arithmetic ------

-- TODO float operations
-- TODO logarithmic number system?


------ conversion ------

zext :: Scale -> Operand -> Scale -> Operand -> Instr
zext scaleA a scaleB b = passThrough $ \thread -> do
    a ::Integer <- ldu thread (scaleA, a)
    stu thread (scaleB, b) a

sext :: Scale -> Operand -> Scale -> Operand -> Instr
sext scaleA a scaleB b = passThrough $ \thread -> do
    a ::Integer <- lds thread (scaleA, a)
    sts thread (scaleB, b) a

-- TODO conversions (float <-> fixed)


------ atomics ------

-- TODO atomics (CAS, atomic inc/add, xchg)


------ system ------

-- TODO syscall operations (i/o, get heap memory, manage threads)
retireThread :: Thread -> IO (Maybe Thread)
retireThread = const $ pure Nothing

