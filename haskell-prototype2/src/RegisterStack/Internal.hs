-- TODO split into an internal and a clean export
module RegisterStack.Internal where

import Foreign.Storable (Storable, sizeOf)
import Data.Binary.Get
import Data.Binary.Put

import Data.IORef

import Control.Monad

import Memory

{-
                               ___________
                           0  | fixed
                              |___________
               _numFixedRegs  |
                              |___________
                 _windowBase  |
                              | window
                              |___________
_windowBase + _numWindowRegs  |
                              |
                              |
                              |___________
                    _numRegs
-}

data RegisterStack = Regs
    { _numRegs :: !Int
    , _windowBase :: !(IORef Int)
    , _numWindowRegs :: !Int
    , _numFixedRegs :: !Int

    , _registerSize :: !Int
    , _regs :: !Memory
    }

-- TODO make this an exception type
data RegisterException
    = BadConfig
    | WindowFault
    | RegisterAccessFault


-- FIXME better argument type
newRegisterStack :: (Int, Int, Int, Int) -> IO RegisterStack
newRegisterStack (_registerSize, _numRegs, _numAvailableRegs, _numFixedRegs) = do
    when (any not preconditions) $ error "bad RegisterStack configuration"
    let _numWindowRegs = _numAvailableRegs - _numFixedRegs
    _regs <- newMemory (fromIntegral $ _registerSize * _numRegs)
    _windowBase <- newIORef _numFixedRegs
    pure Regs{..}
    where
    preconditions =
        [ 0 < _registerSize
        , 0 <= _numFixedRegs
        , _numFixedRegs <= _numAvailableRegs
        , _numAvailableRegs <= _numRegs
        ]


setWindow :: RegisterStack -> Int -> IO ()
setWindow Regs{..} fp' = do
    when (any not $ postconditions fp' ) $ error "register stack window fault"
    writeIORef _windowBase fp'
    where
    postconditions fp' =
        [ fp' >= _numFixedRegs
        , fp' + _numWindowRegs <= _numRegs
        ]

moveWindow :: RegisterStack -> Int -> IO ()
moveWindow regs@Regs{..} amount = do
    fp0 <- readIORef _windowBase
    let fp' = fp0 + amount
    setWindow regs fp'


getReg :: forall a. Storable a => Get a -> RegisterStack -> Int -> IO a
getReg get regs@Regs{..} i = do
    addr <- _registerAddress regs (i, size)
    getMemory get _regs addr
    where
    size = sizeOf (undefined :: a)

putReg :: forall a. Storable a => (a -> Put) -> RegisterStack -> Int -> a -> IO ()
putReg put regs@Regs{..} i x = do
    addr <- _registerAddress regs (i, size)
    putMemory put _regs addr x
    where
    size = sizeOf (undefined :: a)

{-# INLINE _registerAddress #-}
_registerAddress :: RegisterStack -> (Int, Int) -> IO Addr
_registerAddress Regs{..} (i, size) = do
    when (any not preconditions) $ error "register access fault"
    let inFixed = pure $ i
        inWindow = ((i - _numFixedRegs) +) <$> readIORef _windowBase
        offsetInReg = _registerSize - size
    regAddr <- if i < _numFixedRegs then inFixed else inWindow
    pure $ _registerSize * regAddr + offsetInReg
    where
    preconditions =
        [ 0 <= size
        , size <= _registerSize
        , 0 <= i
        , i < _numWindowRegs + _numFixedRegs
        ]
