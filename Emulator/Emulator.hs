{-# Language
        GeneralizedNewtypeDeriving
    #-}
-- | An emulator of the computer
module Emulator where


import Control.Monad.Error
import Control.Monad.State
import Data.Bits
import Data.Function (on)
import Data.List (intersperse, sortBy)
import Data.Maybe (fromJust)


import Assembly
import Disassembler
import MachineInteger


-- * The emulated machine state


-- | Machine data
data Machine
    = Machine
        { m_memory :: [(MachineInteger, MachineInteger)]
        }


instance Show Machine where
    show m = concat $
                [ "Machine\n"
                , "    { "
                ] ++
                intersperse "    , " (map p (m_memory m)) ++
                [ "}" ]
      where p :: (MachineInteger, MachineInteger) -> String
            p (a, v) = let e = if a < 8
                                then " (" ++ show (toEnum $ fromMachineInteger a :: Register) ++ ") "
                                else " " ++ q v
                       in show a ++ ": " ++ show v ++ e ++ "\n"
            q :: MachineInteger -> String
            q mi = maybe "" show $ disassemble mi


-- | A function for memory-mapped registers
registerMemory :: Register -> MachineInteger
registerMemory R0 = 0
registerMemory R1 = 1
registerMemory R2 = 2
registerMemory R3 = 3
registerMemory R4 = 4
registerMemory R5 = 5
registerMemory R6 = 6
registerMemory R7 = 7



-- | The instruction pointer
ip :: Register
ip = R0


-- | The flags register
flagsR :: Register
flagsR = R1


-- | When given a bitfield from the flags register, indicates whether or not
--   the 'Conditional' bit is set.
conditional :: MachineInteger -> Bool
conditional (MachineInteger i) = i .&. 1 == 1


-- | Sets the 'Conditional' bit in the flags register
setConditional :: MachineInteger -> Bool -> MachineInteger
setConditional (MachineInteger i) b =
    MachineInteger $ i .&. 0xffe .|. (if b then 1 else 0)


-- * A monad for machine state


-- | Errors in the machine
data MachineError
    = OtherError String
    | Decorated Machine MachineError
    | CannotDeref MachineInteger
    | InvalidInstr MachineInteger
  deriving Show


instance Error MachineError where
    strMsg = OtherError    


-- | A monad for maintaining machine state
newtype MachineStateT m a = MachineStateT (StateT Machine (ErrorT MachineError m) a)
  deriving (Monad, MonadIO)


-- | Run an action in the emulator
runEmulatorT :: Monad m
                    => MachineStateT m a
                    -> Machine
                    -> m (Either MachineError (a, Machine))
runEmulatorT (MachineStateT action) state = runErrorT (runStateT action state)


-- | Getting the machine state
getMachine :: Monad m => MachineStateT m Machine
getMachine = MachineStateT get


-- | Putting the machine state
putMachine :: Monad m => Machine -> MachineStateT m ()
putMachine = MachineStateT . put


-- | Modifying the machine state
modifyMachine :: Monad m => (Machine -> Machine) -> MachineStateT m ()
modifyMachine = MachineStateT . modify


-- | Throws an error
machineError :: Monad m => MachineError -> MachineStateT m a
machineError err =
     do s <- getMachine
        MachineStateT $ lift $ throwError $ Decorated s err


-- | Dereference a pointer
deref :: Monad m => MachineInteger -> MachineStateT m MachineInteger
deref ptr =
     do memory <- getMachine >>= return . m_memory
        case lookup ptr memory of
            Nothing -> machineError (CannotDeref ptr)
            Just da -> return da


-- | Store at a pointer
store :: Monad m => MachineInteger -> MachineInteger -> MachineStateT m ()
store ptr val = modifyMachine (\ms -> ms{ m_memory = ins $ m_memory ms })
  where ins mem = sortBy (compare `on` fst) $
                    (ptr, val) : [ x | x <- mem, fst x /= ptr ]


-- * Instructions


-- | Increments the instruction pointer by 1
incIp :: Monad m => MachineStateT m ()
incIp =
     do i <- deref (registerMemory ip)
        store (registerMemory ip) (i+1)


-- | Fetches the data pointed at by 'ip'
fetch :: Monad m => MachineStateT m MachineInteger
fetch = deref (registerMemory ip) >>= deref


-- | Executes an instruction
execute :: Monad m => MachineInteger -> MachineStateT m ()
execute instr'@(MachineInteger instr) =
    let opcode = instr `shiftR` 6 .&. 0x3f
    in case lookup opcode executors of
        Nothing -> machineError (InvalidInstr instr')
        Just fn -> fn instr
  where executors = mathFn ++ logicFn ++ loadFn ++ specialFn
        mathFn =
            [ (c `shiftL` 3 .|. fn, mathExecute (c == 1) fn)
            | c <- [0, 1], fn <- [0 .. 7]
            ]
        logicFn =
            [ (1 `shiftL` 4 .|. fn, logicExecute fn)
            | fn <- [0 .. 0xf]
            ]
        loadFn =
            [ (2 `shiftL` 4 .|. p `shiftL` 2 .|. r, loadExecute p r)
            | p <- [0 .. 3], r <- [0 .. 3]
            ]
        specialFn =
            [ (0x32, eqExecute)
            , (0x34, rotExecute), (0x35, rotExecute)
            ] ++
            [ (0x1b `shiftL` 1 .|. c, immMathExecute (c == 1))
            | c <- [0, 1], fn <- [0, 1]
            ]


-- | Decodes a register
decodeReg :: Integer -> Register
decodeReg i = toEnum $ fromIntegral $ i .&. 0x7


-- | Decodes the destination register
decodeDst :: Integer -> Register
decodeDst = decodeReg


-- | Decodes the source register
decodeSrc :: Integer -> Register
decodeSrc = decodeReg . (`shiftR` 3)


-- | Executes a math primitive
mathExecute :: Monad m
                    => Conditional              -- ^ Conditional control
                    -> Integer                  -- ^ Function
                    -> Integer                  -- ^ The instruction
                    -> MachineStateT m ()
mathExecute c fn instr =
 do fl <- deref $ registerMemory flagsR
    let aR = decodeSrc instr
        bR = decodeDst instr
    a <- deref $ registerMemory aR
    b <- deref $ registerMemory bR
    when (not c || (c && conditional fl)) $
         case fn of
            0 -> store (registerMemory bR) (a + b)
            1 -> store (registerMemory bR) (a - b)
            2 -> store (registerMemory bR) (b + a)
            3 -> store (registerMemory bR) (b - 1)
            4 -> store (registerMemory bR) (b + 1)
            5 -> store (registerMemory bR) (negate b)
            6 -> store (registerMemory flagsR) (setConditional fl (signedLt a b))
            7 -> store (registerMemory flagsR) (setConditional fl (unsignedLt a b))
            _ -> fail $ "bad case in mathExecute: " ++ show fn
    incIp
  where signedLt :: MachineInteger -> MachineInteger -> Conditional
        signedLt a b   = let MachineInteger d = a - b
                         in d .&. 0x800 /= 0
        unsignedLt :: MachineInteger -> MachineInteger -> Conditional
        unsignedLt a b
            | a == 0 && b == 0 = False
            | a == 0 = True
            | b == 0 = False
            | otherwise = let maxBit k = fromMachineInteger $
                                         fromJust $
                                         lookup True [ (testBit k q, b)
                                                     | q <- reverse [0 .. 11]
                                                     ]
                              mba = maxBit a
                              mbb = maxBit b
                          in if mba == mbb
                              then unsignedLt (clearBit a mba) (clearBit b mbb)
                              else mba < mbb


-- | Executes a logic primitive
logicExecute :: Monad m
                    => Integer                  -- ^ Function
                    -> Integer                  -- ^ The instruction
                    -> MachineStateT m ()
logicExecute fn instr =
     do let aR = decodeSrc instr
            bR = decodeDst instr
        a <- deref $ registerMemory aR
        b <- deref $ registerMemory bR
        case fn of
            0x0 -> store (registerMemory bR) (complement b)
            0x1 -> store (registerMemory bR) (complement $ a .|. b)
            0x2 -> store (registerMemory bR) (a .&. complement b)
            0x3 -> store (registerMemory bR) 0
            0x4 -> store (registerMemory bR) (complement $ a .&. b)
            0x5 -> store (registerMemory bR) (complement a)
            0x6 -> store (registerMemory bR) (xor a b)
            0x7 -> store (registerMemory bR) (complement a .&. b)
            0x8 -> store (registerMemory bR) (complement b .|. a)
            0x9 -> store (registerMemory bR) (complement $ a `xor` b)
            0xa -> store (registerMemory bR) a
            0xb -> store (registerMemory bR) (a .&. b)
            0xc -> store (registerMemory bR) 0xfff
            0xd -> store (registerMemory bR) (complement a .|. b)
            0xe -> store (registerMemory bR) (a .|. b)
            0xf -> do fl <- deref (registerMemory flagsR)
                      store (registerMemory flagsR) (setConditional fl (b == 0))
            _ -> fail $ "bad case in logicExecute: " ++ show fn
        incIp


-- | Executes a load instruction
loadExecute :: Monad m
                    => Integer                  -- ^ Mode for the first arg
                    -> Integer                  -- ^ Mode for the second arg
                    -> Integer                  -- ^ The instruction
                    -> MachineStateT m ()
loadExecute ma mb instr =
     do incIp
        let aR = decodeSrc instr
            bR = decodeDst instr
        da <- getReg ma aR
        putReg mb bR da
  where getReg 0 r = deref $ registerMemory r
        getReg 1 r = deref (registerMemory r) >>= deref
        getReg 2 r = do rv <- deref (registerMemory r)
                        da <- deref rv
                        store (registerMemory r) (rv + 1)
                        return da
        getReg 3 r = do rv <- deref (registerMemory r) >>= return . (subtract 1)
                        da <- deref rv
                        store (registerMemory r) rv
                        return da
        getReg x _ = fail $ "loadExecute:getReg: bad mode " ++ show x
        putReg 0 r v = store (registerMemory r) v
        putReg 1 r v = deref (registerMemory r) >>= flip store v
        putReg 2 r v = do rv <- deref (registerMemory r)
                          store rv v
                          store (registerMemory r) (rv + 1)
        putReg 3 r v = do rv <- deref (registerMemory r) >>= return . (subtract 1)
                          store rv v
                          store (registerMemory r) rv
        putReg x _ _ = fail $ "loadExecute:putReg: bad mode " ++ show x


-- | Executes an equality check
eqExecute :: Monad m => Integer -> MachineStateT m ()
eqExecute instr =
     do let aR = decodeSrc instr
            bR = decodeDst instr
        a <- deref $ registerMemory aR
        b <- deref $ registerMemory bR
        fl <- deref $ registerMemory flagsR
        store (registerMemory flagsR) (setConditional fl (a == b))
        incIp


-- | Execute a rotation
rotExecute :: Monad m => Integer -> MachineStateT m ()
rotExecute instr =
     do let rot = fromIntegral $ instr `shiftR` 3 .&. 0xf
            bR = decodeDst instr
        b <- deref $ registerMemory bR
        store (registerMemory bR) (b `rotateR` rot)
        incIp


-- | Execute an immediate math instruction
immMathExecute :: Monad m
                    => Conditional              -- ^ Conditional control
                    -> Integer                  -- ^ The instruction
                    -> MachineStateT m ()
immMathExecute c instr =
     do let fn = instr `shiftR` 3 .&. 0x7
        incIp
        fl <- deref $ registerMemory flagsR
        let bR = decodeDst instr
        a <- fetch
        b <- deref $ registerMemory bR
        when (not c || (c && conditional fl)) $
             case fn of
                0 -> store (registerMemory bR) (a+b)
                1 -> store (registerMemory bR) (b-a)
                2 -> store (registerMemory bR) (b .|. a)
                3 -> store (registerMemory bR) (b .|. a)
                _ -> fail $ "bad case in immMathExecute: " ++ show fn
        incIp


