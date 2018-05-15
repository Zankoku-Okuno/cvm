module Metadata where

import Types


data MachineMetadata = MachineMetadata
    { addrSize :: !Int
    , bigEndian :: !Bool
    , bitSizes :: ![(Int, String)]
    -- TODO number of cores
    }

hostMetadata :: MachineMetadata
hostMetadata = MachineMetadata {..}
    where
    addrSize = sizeOf (undefined :: Addr)
    bigEndian =
        let testData = 1
            written = runPut $ putWord16host testData
            readBack = flip runGet written $ getWord16be
        in readBack == testData
    bitSizes =
        [ (8 , "byte")
        , (16, "wide")
        , (32, "quad")
        , (64, "oct" )
        ]
