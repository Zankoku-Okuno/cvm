module Main where

import qualified Data.ByteString.Lazy as LBS
import Data.Binary
import Foreign.Storable
import Numeric (showHex)
import Data.IORef
import Data.List
import Data.Array.MArray
import Data.Array.IO
import Text.Bytedump
import Data.Word
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception

import Memory
import Machine
import Addressing




------------------ VM Operations ------------------

passThrough action v = action v >> pure (Just v)

-- FIXME this is only an interim debug facility, I need to be able to inspect all threads on the vm from outside the vm
printThread :: Instr
printThread = passThrough print


------ moving data ------
mov :: Scale -> Operand -> Operand -> Instr
mov scale src dst = passThrough $ \thread ->
    ldb thread (scale, src) >>= stb thread (scale, dst)

ld_sp :: Operand -> Instr
ld_sp dst = passThrough $ \thread -> do
    stu thread (AddrSize, dst) (sp thread)
st_sp :: Word -> Instr
st_sp src thread = pure $
    if src > spLimit thread
        then Just thread{sp = src}
        else error "STACK OVERFLOW"

ld_fp :: Operand -> Instr
ld_fp dst = passThrough $ \thread -> do
    stu thread (AddrSize, dst) (fp thread)
st_fp :: Word -> Instr
st_fp src thread = pure $ Just thread{fp = src}


------ jumping ------

jmp :: Operand -> Instr
jmp src@(Immediate _) thread = do -- absolute jump
    offset <- lds thread (AddrSize, src)
    let ip' = ip thread + offset
    pure $ Just thread{ip = ip'}
jmp src thread = do
    ip' <- lds thread (AddrSize, src)
    pure $ Just thread{ip = ip'}

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

-- TODO call? tailcall? return?


------  logic ------

-- TODO logical operations


------ integer arithmetic ------

add :: Scale -> Operand -> Operand -> Maybe (Operand) -> Instr
add scale src dst carry = passThrough $ \thread -> do
    a <- ldu thread (scale, src)
    b <- ldu thread (scale, dst)
    cin <- case carry of
            Nothing -> pure 0
            Just src -> ldu thread (scale, src)
    let c :: Integer = a + b + cin
        cout = c - maxValue scale
    stu thread (scale, dst) c
    case carry of
        Nothing -> pure ()
        Just dst -> stu thread (scale, dst) cout
-- TODO integer operations


------ floating-point arithmetic ------

-- TODO float operations
-- TODO logarithmic number system?
-- TODO BCD/packed BCD?

------ conversion ------

-- TODO conversions


------ atomics ------

-- TODO atomics (CAS, atomic inc/add)


------ system ------

-- TODO syscall operations (i/o, get heap memory, manage threads)
retireThread :: Thread -> IO (Maybe Thread)
retireThread = const $ pure Nothing


------------------ Play Around ------------------

main :: IO ()
main = do
    let code = [ -- TODO don't hardcode the instructions
                -- var i: byte @ R1
                -- var addr: *byte @ R2
                  mov Byte (Immediate $ encode (0::Word8)) (GPR 1) -- i := 0
                , mov AddrSize (Immediate $ encode (0::Word)) (GPR 2) -- addr := 0
                -- -- label loop @ 2
                , mov Byte (GPR 1) (Deferred (GPR 2)) -- *addr := i
                , add Byte (Immediate $ encode (1::Word8)) (GPR 1) Nothing -- ++i
                , add AddrSize (Immediate $ encode (1::Word)) (GPR 2) Nothing -- ++addr
                , jmp_lt (Immediate $ encode ((-4)::Int)) Byte (GPR 1) (Immediate $ encode (10::Word8)) -- if i < 10, goto loop
                -- @ loop+4
                , retireThread
                ]
        config = MemConfig
                    { rootStackSize = 128
                    , threadStackSize = 0
                    , maxThreads = 1
                    , heapSize = 256
                    }
    m <- newMachine config code
    (_, wait) <- runRoot m
    wait >>= \case
        Nothing -> pure ()
        Just exn -> print exn
    let (heapAddrs, stackAddrs) = addresses (mem m)
    putStrLn "HEAP:"
    heapdump <- readBytes (mem m) (fst heapAddrs) (fromIntegral $ rangeSize heapAddrs)
    putStrLn $ dumpLBS heapdump
    putStrLn "STACK:"

    stackdump <- readBytes (mem m) (fst stackAddrs) (fromIntegral $ rangeSize stackAddrs)
    putStrLn $ dumpLBS stackdump