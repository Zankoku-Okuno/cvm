module Machine
    ( Instr
    , Machine(..) -- FIXME be more discerning with the exports here
    , newMachine
    , Thread(..) -- FIXME be more discerning with the exports here
    , runRoot
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Numeric (showHex)

import Memory
import Registers


data Machine = Machine
    { config :: MemConfig
    , code :: [Instr]
    , mem :: Mem
    , threads :: MVar [Maybe (ThreadId, Thread)]
    , atomic_request_channel_ :: Chan AtomicCtrl
    , atomic_memory_controller_ :: ThreadId
    -- TODO some way to identify all threads & their state
    }
data AtomicCtrl =
      Request (Word, Word) (MVar ())
    | Release Word

newMachine :: MemConfig -> [Instr] -> IO Machine
newMachine config code = do
    mem <- newMem config
    atomic_request_channel_ <- newChan
    atomic_memory_controller_ <- forkIO $ atomicMemoryController atomic_request_channel_
    threads <- newMVar $ replicate (fromIntegral $ maxThreads config) Nothing
    pure Machine{..}
    where

atomicMemoryController :: Chan AtomicCtrl -> IO ()
atomicMemoryController chan = do
    lockedList <- newIORef []
    pendingList <- newIORef []
    let loop = readChan chan >>= handle >> loop
        handle req@(Request (addr, size) notify) = do
            locked <- readIORef lockedList
            let range = (addr, addr+size)
            if locked `canLock` range
            then do
                modifyIORef lockedList (range:)
                notify `putMVar` ()
            else modifyIORef pendingList (req:)
        handle (Release addr) = do
            modifyIORef lockedList $ unlock addr
            pending <- readIORef pendingList
            pendingList `writeIORef` []
            forM_ (reverse pending) $ writeChan chan -- FIXME I hope this can't deadlock; besides, it's probably not very efficient
    loop
    where
    unlock :: Word -> [(Word, Word)] -> [(Word, Word)]
    unlock addr [] = []
    unlock addr (hd@(addr', _):rest) = if addr == addr' then rest else unlock addr (hd:rest)
    canLock :: [(Word, Word)] -> (Word, Word) -> Bool
    canLock current new = not $ any (overlaps new) current
    overlaps :: (Word, Word) -> (Word, Word) -> Bool
    -- NOTE: assumes lo <= hi and lo' <= hi'
    overlaps (lo, hi) (lo', hi') =  hi <= lo'
                                 || hi' <= lo


data Thread = Thread
    { ip :: Int
    , spLimit :: Word -- where there's a stack overflow
    , gprs :: RegisterFile
    , machine :: Machine
    }
instance Show Thread where
    show Thread{..} = concat [
          "Thread{ip = ", showHex ip "", ", "
        , "spLimit = ", showHex spLimit ""
        , "}"
        ]

runThread :: Thread -> IO ()
runThread thread@Thread{..} = do
        let Machine{..} = machine
            instr = code !! ip
        thread' <- instr thread{ip = ip + 1}
        maybe (pure ()) runThread thread'

type Instr = Thread -> IO (Maybe Thread) -- FIXME this needs to be a showable data type for debugging (means I'll need a decode step)


runRoot :: Machine -> IO (ThreadId, IO (Maybe SomeException))
runRoot machine@Machine{..} = modifyMVar threads $ \case
    [] -> error "zero max threads"
    (Just _:rest) -> error "machine already running"
    (Nothing:rest) -> do
        gprs <- newRegisterFile
        let root = Thread
                    { ip = 0
                    , spLimit = maxBound - rootStackSize config + 1 -- FIXME ask memory for this
                    , gprs = gprs
                    , machine = machine
                    }
        doneVar <- newEmptyMVar
        rootId <- forkFinally (runThread root) (retire doneVar)
        pure (Just (rootId, root) : rest, (rootId, readMVar doneVar))
    where
    retire doneVar (Right _) = putMVar doneVar Nothing
    retire doneVar (Left exn) = putMVar doneVar (Just exn)
