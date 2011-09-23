-- | A disassembler for the computer
module Emulator where


import Data.Bits


import Assembly
import MachineInteger


-- * The disassembler


-- | Entry to the disassembler
disassemble :: MachineInteger -> Maybe Instruction
disassemble (MachineInteger instr)
    | instr `shiftR` 10 == 0 = disasMath instr
    | instr `shiftR` 10 == 1 = disasLogic instr
    | instr `shiftR` 10 == 2 = disasLoad instr
    | instr `shiftR` 10 == 3 = disasGeneric instr
    | otherwise = Nothing


-- | Disassemble a math instruction
disasMath :: Integer -> Maybe Instruction
disasMath instr =
    let c = instr .&. 0x200 /= 0
        src = disasSrc instr
        dst = disasDst instr
        mf = lookup (instr .&. 0x1c0 `shiftR` 6)
                    [ (0, Add)
                    , (1, Sub)
                    , (2, Nsub)
                    , (3, \c _ y -> Dec c y)
                    , (4, \c _ y -> Inc c y)
                    , (5, \c _ y -> Neg c y)
                    , (6, SLt)
                    , (7, ULt)
                    ]
    in case mf of
        Just fn -> Just $ fn c src dst
        Nothing -> Nothing


-- | Disassemble a logic instruction
disasLogic :: Integer -> Maybe Instruction
disasLogic instr =
    let src = disasSrc instr
        dst = disasDst instr
        mf = lookup (instr .&. 0x3c0 `shiftR` 6)
                    [ (0x0, \_ y -> Not y)
                    , (0x1, Nor)
                    , (0x2, AndN2)
                    , (0x3, \_ y -> Clr y)
                    , (0x4, Nand)
                    , (0x5, CopyN1)
                    , (0x6, Xor)
                    , (0x7, AndN1)
                    , (0x8, OrN2)
                    , (0x9, Xnor)
                    , (0xa, Copy)
                    , (0xb, And)
                    , (0xc, \_ y -> Set y)
                    , (0xd, OrN1)
                    , (0xe, Or)
                    , (0xf, \_ y -> Tst y)
                    ]
    in case mf of
        Just fn -> Just $ fn src dst
        Nothing -> Nothing


-- | Disassemble a load instruction
disasLoad :: Integer -> Maybe Instruction
disasLoad instr =
    let src = disasSrc instr
        dst = disasDst instr
        src_mode = instr .&. 0x300 `shiftR` 8
        dst_mode = instr .&. 0x0c0 `shiftR` 6
        moded mode | mode == 0 = Direct
                   | mode == 1 = Indirect
                   | mode == 2 = IndirectInc
                   | mode == 3 = IndirectDec
        src' = moded src_mode src
        dst' = moded dst_mode dst
    in Just $ Mov src' dst'


-- | Disassemble a generic instruction
disasGeneric :: Integer -> Maybe Instruction
disasGeneric instr
    | instr .&. 0xfc0 == 0xc80 = let src = disasSrc instr
                                     dst = disasDst instr
                                 in Just $ Eq src dst
    | instr .&. 0xf80 == 0xd00 = let k = instr .&. 0x078 `shiftR` 3
                                     dst = disasDst instr
                                 in Just $ Rot k dst
    | instr .&. 0xf80 == 0xd80 = let c = instr .&. 0x040 /= 0
                                     dst = disasDst instr
                                     mf = lookup (instr .&. 0x038 `shiftR` 3)
                                                 [ (0, IAdd)
                                                 , (1, INSub)
                                                 , (2, IOr)
                                                 , (3, IAnd)
                                                 ]
                                 in case mf of
                                        Just fn -> Just $ fn c (Right "") dst
                                        Nothing -> Nothing
    | otherwise = Nothing


-- * Disassembly helpers


-- | Disassembles the source register
disasSrc :: Integer -> Register
disasSrc = disasDst . (`shiftR` 3)


-- | Disassembles the destination register
disasDst :: Integer -> Register
disasDst instr = toEnum $ fromIntegral $ instr .&. 0x007


