-- | Machine words
module MachineInteger where


import Data.Bits


import Assembly


-- * Machine words


-- | A wrapper for machine words
newtype MachineInteger = MachineInteger Integer


-- | Convert an 'AssembledWord' to a 'MachineInteger'
awTomi :: AssembledWord -> MachineInteger
awTomi (AssembledLiter i) = MachineInteger i
awTomi (AssembledInstr i) = MachineInteger i


-- | A 'fromIntegral' for 'MachineInteger's
fromMachineInteger :: Num b => MachineInteger -> b
fromMachineInteger (MachineInteger a) = fromIntegral a


-- | Lifts an integer function to 'MachineInteger's
miLift :: (Integer -> Integer)
            -> MachineInteger
            -> MachineInteger
miLift f (MachineInteger a) = MachineInteger $ f a .&. 0xfff


-- | Lifts an integer function to 'MachineInteger's
miLift2 :: (Integer -> Integer -> Integer)
            -> MachineInteger
            -> MachineInteger
            -> MachineInteger
miLift2 f (MachineInteger a) (MachineInteger b) = MachineInteger $ (a `f` b) .&. 0xfff


instance Show MachineInteger where
    show (MachineInteger i) = let bk k | (1 `shiftL` k) .&. i == 0 = '0'
                                       | otherwise = '1'
                              in map bk (reverse [0 .. 11])


instance Eq MachineInteger where
    (MachineInteger a) == (MachineInteger b) = a .&. 0xfff == b .&. 0xfff


instance Ord MachineInteger where
    compare (MachineInteger a) (MachineInteger b) = compare (a .&. 0xfff) (b .&. 0xfff)


instance Num MachineInteger where
    a + b = miLift2 (+) a b
    a * b = miLift2 (*) a b
    a - b = miLift2 (-) a b
    negate a = miLift negate a
    abs a = miLift abs a
    signum a = miLift signum a
    fromInteger = MachineInteger


instance Bits MachineInteger where
    a .&. b = miLift2 (.&.) a b
    a .|. b = miLift2 (.|.) a b
    xor = miLift2 xor
    complement = miLift complement
    shift a b = miLift (flip shift b) a
    rotate a b = miLift (flip rotate b) a
    bitSize _ = 12
    isSigned _ = False


