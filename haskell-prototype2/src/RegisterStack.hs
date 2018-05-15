-- TODO split into an internal and a clean export
module RegisterStack
    ( RegisterStack
    , RegisterException(..)
    , newRegisterStack
    , setWindow, moveWindow
    , getReg, putReg
    ) where

import RegisterStack.Internal
