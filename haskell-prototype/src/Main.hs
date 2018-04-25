module Main where

import Data.Binary
import Data.Array.MArray
import Text.Bytedump
import Data.Int
import Data.Word

import Addressing
import InstructionSet
import Machine
import Memory
import Registers


------ Debug instructions ------

-- FIXME this is only an interim debug facility, I need to be able to inspect all threads on the vm from outside the vm
printThread :: Instr
printThread = passThrough print

pauseAndPrint :: Scale -> Operand -> Instr
pauseAndPrint scale src = passThrough $ \thread -> do
    bytes <- ldb thread (scale, src)
    putStrLn $ dumpLBS bytes
    _ <- getLine
    pure ()


------------------ Play Around ------------------

count_program = [
                -- var i: byte @ R1
                -- var addr: *byte @ R2
                  mov Byte (GPR 0) (GPR 1) -- i := 0
                , mov AddrSize (GPR 0) (GPR 2) -- addr := 0
                -- -- label loop @ 2
                , mov Byte (GPR 1) (Deferred (GPR 2)) -- *addr := i
                , add Byte (Immediate $ encode (1::Word8)) (GPR 1) (GPR 0) -- ++i
                , add AddrSize (Immediate $ encode (1::Word)) (GPR 2) (GPR 0) -- ++addr
                , jmp_lt (Immediate $ encode ((-4)::Int)) Byte (GPR 1) (Immediate $ encode (10::Word8)) -- if i < 10, goto loop
                -- @ loop+4
                , retireThread
                ]

main :: IO ()
main = do
    -- TODO don't hardcode the instructions
    let code = [
                  mov Byte (Immediate $ encode (0::Word8)) (GPR 1) -- a = 0
                , mov Byte (GPR 0) (GPR 2) -- carry = 0
                , sub Byte (Immediate $ encode (1::Word8)) (GPR 1) (GPR 2) -- a -= 1
                -- , pauseAndPrint Byte (GPR 1)
                -- , pauseAndPrint Byte (GPR 2)
                , sub Byte (Immediate $ encode (0::Word8)) (GPR 1) (GPR 2) -- a -= 0
                -- , pauseAndPrint Byte (GPR 1)
                -- , pauseAndPrint Byte (GPR 2)
                -- , mov DblByte (Immediate $ encode (3::Word16)) (Offset sp_reg (-2))
                , mov Byte (Immediate $ encode (0::Word8)) (Offset sp_reg (-3))
                , sub Byte (Immediate $ encode (1::Word8)) (Offset sp_reg (-1)) (Offset sp_reg (-3))
                , sub Byte (Immediate $ encode (0::Word8)) (Offset sp_reg (-2)) (Offset sp_reg (-3))
                , jmp_eq (Immediate $ encode (1::Int)) Byte (GPR 0) (Offset sp_reg (-3))
                , mov Byte (Immediate $ encode (0xff::Word8)) (Offset sp_reg (-3))
                , sext Byte (Offset sp_reg (-3)) DblByte (Offset sp_reg (-4))
                , neg QuadByte (Offset sp_reg (-4)) (Offset sp_reg (-8))
                
                , mov Byte (Immediate $ encode (0x81::Word8)) (Offset sp_reg (-9))
                , rot (Immediate $ encode (-1::Int8)) Byte (Offset sp_reg (-9))

                , mov OctByte (Immediate $ encode (0x0102030405060708::Word64)) (Deferred $ Immediate $ encode (0::Word))
                , swap Byte OctByte (Deferred $ Immediate $ encode (0::Word))
                , swap DblByte OctByte (Deferred $ Immediate $ encode (0::Word))
                , popcnt OctByte (Deferred $ Immediate $ encode (0::Word)) (Deferred $ Immediate $ encode (8::Word))

                , mov Byte (Immediate $ encode (0x78::Word8)) (Deferred $ Immediate $ encode (0x10::Word))
                , clz Byte (Deferred $ Immediate $ encode (0x10::Word)) (Deferred $ Immediate $ encode (0x11::Word))
                , ctz Byte (Deferred $ Immediate $ encode (0x10::Word)) (Deferred $ Immediate $ encode (0x12::Word))
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