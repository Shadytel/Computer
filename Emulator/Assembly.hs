-- | The assembly language
module Assembly where


import Data.Bits



-- * Abstract syntax


-- | An address
type Address = Integer


-- | Registers
data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  deriving (Enum, Eq, Show)


-- | Means of using registers
data RegisterMode
    = Direct Register
    | Indirect Register
    | IndirectInc Register
    | IndirectDec Register
  deriving (Eq, Show)


-- | Used to indicate whether an instruction should only execute if the
--   \"conditional\" flag is set.
type Conditional = Bool


-- | Instructions
data Instruction
    -- Math operations
    = Add Conditional Register Register
    | Jmp Conditional String
    | Sub Conditional Register Register
    | Nsub Conditional Register Register
    | Dec Conditional Register
    | Inc Conditional Register
    | Neg Conditional Register
    | SLt Conditional Register Register
    | ULt Conditional Register Register
    | Eq Register Register
    -- Immediate math operations
    | IAdd Conditional (Either Integer String) Register
    | INSub Conditional (Either Integer String) Register
    | IOr Conditional (Either Integer String) Register
    | IAnd Conditional (Either Integer String) Register
    -- Logic operations
    | Not Register
    | Nor Register Register
    | AndN2 Register Register
    | Clr Register
    | Nand Register Register
    | CopyN1 Register Register
    | Xor Register Register
    | AndN1 Register Register
    | OrN2 Register Register
    | Xnor Register Register
    | Copy Register Register
    | And Register Register
    | Set Register
    | OrN1 Register Register
    | Or Register Register
    | Tst Register
    -- Load
    | Mov RegisterMode RegisterMode
    | Movi Integer RegisterMode
    | Movil String RegisterMode
    -- Rotate
    | Rot Integer Register
    -- Literals
    | Liti Integer
    | Litl String
  deriving (Eq, Show)


-- | A line in a listing
data CodeWord
    = Label String
    | Instr Instruction
  deriving (Eq, Show)


-- | A compiled word or literal
data AssembledWord
    = AssembledInstr Integer
    | AssembledLiter Integer
  deriving Eq


instance Show AssembledWord where
    show a = let p i = let bk k | (1 `shiftL` k) .&. i == 0 = '0'
                                | otherwise = '1'
                       in map bk (reverse [0 .. 11])
             in case a of
                    AssembledInstr i -> "i " ++ p i
                    AssembledLiter i -> "l " ++ p i



