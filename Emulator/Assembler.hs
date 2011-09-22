-- | The assembler
module Assembler where


import Control.Monad
import Data.Bits
import Data.List
import Data.Maybe (isJust, fromJust, mapMaybe)
import System.IO
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Text.Groom


import Assembly


-- * The parser


-- | The parser type
type AssemblyParser u a = Parsec String u a


-- | Running the parser
runAssemblyParser :: Parsec String u a
                        -> u
                        -> FilePath
                        -> IO (Either ParseError a)
runAssemblyParser parser state path =
     do c <- withFile path ReadMode $ \h ->
                 do c <- hGetContents h
                    when (null c) (fail "empty file")
                    return c
        return (runParser parser state path c)


-- | Parses an assembly file
parseAssemblyFile :: FilePath -> IO (Either ParseError [CodeWord])
parseAssemblyFile path = runAssemblyParser parser () path
  where parser =
         do whiteSpace
            ws <- many1 parseCodeWord
            eof
            return ws


-- | Prints an assembly file
printAssemblyFile :: FilePath -> IO ()
printAssemblyFile path = parseAssemblyFile path >>= putStrLn . groom


-- | Assembles a file
assembleFile :: FilePath -> IO (Either ParseError [Assembled])
assembleFile path =
     do e <- parseAssemblyFile path
        case e of
            Left err  -> return $ Left err
            Right cws -> return $ Right $ assembleListing $ assignAddresses cws


-- ** Basic definitions of the language style


-- | The language style
assemblyStyle :: LanguageDef st
assemblyStyle = emptyDef
                    { Token.commentStart = ""
                    , Token.commentEnd   = ""
                    , Token.commentLine  = ";"
                    , Token.nestedComments = False
                    , Token.identStart = letter <|> oneOf "._"
                    , Token.identLetter = alphaNum <|> oneOf "._"
                    , Token.opStart = Token.opLetter assemblyStyle
                    , Token.opLetter = oneOf "+-:"
                    , Token.reservedOpNames = ["++", "--", ":"]
                    , Token.reservedNames = [ "r0", "r1", "r2", "r3"
                                            , "r4", "r5", "r6", "r7"
                                            , "ip"
                                            , ".lit"
                                            , "eq"
                                            , "mov", "imov", "rot"
                                            , "not", "nor", "and.n2", "clr"
                                            , "nand", "copy.n1", "xor", "and.n1"
                                            , "or.n2", "xnor", "copy", "and"
                                            , "set", "or.n1", "or", "tst"
                                            ]
                                            ++ mathReserved
                                            ++ map ("if." ++ ) mathReserved
                    , Token.caseSensitive = False
                    }
  where mathReserved = [ "jmp"
                       , "add", "sub", "nsub"
                       , "dec", "inc", "neg"
                       , "u.lt", "s.lt"
                       ]


-- | The tokenizer
assembly :: Token.TokenParser st
assembly = Token.makeTokenParser assemblyStyle


-- ** Primitive parsers


-- | Parses whitespace
whiteSpace :: AssemblyParser u ()
whiteSpace = Token.whiteSpace assembly


-- | Parse an identifier
identifier :: AssemblyParser u String
identifier = Token.identifier assembly


-- | Parse a reserved word
reserved :: String -> AssemblyParser u ()
reserved = Token.reserved assembly


-- | Parse a reserved operator
reservedOp :: String -> AssemblyParser u ()
reservedOp = Token.reservedOp assembly


-- | Parse a natural number
natural :: AssemblyParser u Integer
natural = Token.natural assembly


-- | Parses something enclosed in parenthesis
parens :: AssemblyParser u a -> AssemblyParser u a
parens = Token.parens assembly


-- | Parses a colon
colon :: AssemblyParser u String
colon = Token.colon assembly


-- | Parses a comma
comma :: AssemblyParser u String
comma = Token.comma assembly


-- ** Parsing registers and address modes


-- | Parse a register
parseRegister :: AssemblyParser u Register
parseRegister = foldl1 (<|>) (map pr' regs)
  where pr' (s, r) = reserved s >>= const (return r)
        regs = [ ("ip", R0)
               , ("r0", R0), ("r1", R1), ("r2", R2), ("r3", R3)
               , ("r4", R4), ("r5", R5), ("r6", R6), ("r7", R7)
               ]


-- | Parse a register mode
parseRegisterMode :: AssemblyParser u RegisterMode
parseRegisterMode =
    (parseRegister >>= return . Direct) <|>
    parens parseIndirectRegisterMode
  where parseIndirectRegisterMode =
            (do r <- parseRegister
                m <- optionMaybe (reservedOp "++")
                if isJust m
                    then return (IndirectInc r)
                    else return (Indirect r)) <|>
            (do reservedOp "--"
                r <- parseRegister
                return (IndirectDec r))


-- ** Parsing instructions


-- | Parses a math operation
parseMath :: AssemblyParser u Instruction
parseMath = parseJmp <|>
            parseBinary "add" Add <|>
            parseBinary "sub" Sub <|>
            parseBinary "nsub" Nsub <|>
            parseUnary "dec" Dec <|>
            parseUnary "inc" Inc <|>
            parseUnary "neg" Neg <|>
            parseBinary "s.lt" SLt <|>
            parseBinary "u.lt" ULt <|>
            parseImm "iadd" IAdd <|>
            parseImm "insub" INSub
  where parseUnary :: String
                        -> (Conditional -> Register -> Instruction)
                        -> AssemblyParser u Instruction
        parseUnary opcode f =
         do c <- try ((reserved ("if." ++ opcode) >>= const (return True)) <|>
                      (reserved opcode >>= const (return False)))
            r <- parseRegister
            return (f c r)
        parseBinary :: String
                        -> (Conditional -> Register -> Register -> Instruction)
                        -> AssemblyParser u Instruction
        parseBinary opcode f =
         do c <- try ((reserved ("if." ++ opcode) >>= const (return True)) <|>
                      (reserved opcode >>= const (return False)))
            r1 <- parseRegister
            _ <- comma
            r2 <- parseRegister
            return (f c r1 r2)
        parseJmp =
         do c <- try ((reserved ("if.jmp") >>= const (return True)) <|>
                      (reserved "jmp" >>= const (return False)))
            l <- identifier
            return (Jmp c l)
        parseImm opcode f =
         do c <- try ((reserved ("if." ++ opcode) >>= const (return True)) <|>
                      (reserved opcode >>= const (return False)))
            i <- (identifier >>= return . Right) <|> (natural >>= return . Left)
            _ <- comma
            r <- parseRegister
            return (f c i r)


-- | Parses a logic operation
parseLogic :: AssemblyParser u Instruction
parseLogic = parseUnary "not" Not <|>
             parseBinary "nor" Nor <|>
             parseBinary "and.n2" AndN2 <|>
             parseUnary "clr" Clr <|>
             parseBinary "nand" Nand <|>
             parseBinary "copy.n1" CopyN1 <|>
             parseBinary "xor" Xor <|>
             parseBinary "and.n1" AndN1 <|>
             parseBinary "or.n2" OrN2 <|>
             parseBinary "xnor" Xnor <|>
             parseBinary "copy" Copy <|>
             parseBinary "and" And <|>
             parseUnary "set" Set <|>
             parseBinary "or.n1" OrN1 <|>
             parseBinary "or" Or <|>
             parseUnary "tst" Tst
  where parseUnary :: String
                        -> (Register -> Instruction)
                        -> AssemblyParser u Instruction
        parseUnary opcode f =
         do try (reserved opcode)
            r <- parseRegister
            return (f r)
        parseBinary :: String
                        -> (Register -> Register -> Instruction)
                        -> AssemblyParser u Instruction
        parseBinary opcode f =
         do try (reserved opcode)
            r1 <- parseRegister
            _ <- comma
            r2 <- parseRegister
            return (f r1 r2)


-- | Parses a move instruction
parseMov :: AssemblyParser u Instruction
parseMov = parseMov1 <|> parseMov2
  where parseMov1 =
         do try (reserved "movi")
            (do i <- natural
                _ <- comma
                r <- parseRegisterMode
                return (Movi i r)) <|>
             (do l <- identifier
                 _ <- comma
                 r <- parseRegisterMode
                 return (Movil l r))
        parseMov2 =
         do try (reserved "mov")
            r1 <- parseRegisterMode
            _ <- comma
            r2 <- parseRegisterMode
            return (Mov r1 r2)


-- | Parses a rotate instruction
parseRot :: AssemblyParser u Instruction
parseRot =
     do try (reserved "rot")
        i <- natural
        _ <- comma
        r <- parseRegister
        return (Rot i r)


-- | Parses a literal instruction
parseLit :: AssemblyParser u Instruction
parseLit =
     do try (reserved ".lit")
        (natural >>= return . Liti) <|> (identifier >>= return . Litl)


-- | Parses a code word
parseCodeWord :: AssemblyParser u CodeWord
parseCodeWord = (parseMath >>= return . Instr) <|> 
                (parseLogic >>= return . Instr) <|>
                (parseMov >>= return . Instr) <|>
                (parseRot >>= return . Instr) <|>
                (parseLit >>= return . Instr) <|>
                (do i <- identifier
                    _ <- colon
                    return (Label i))


-- * The assembler


-- | Assigns addresses to a list of 'CodeWord's.  This function is wise enough
--   to only increment the address counter on 'Instr's, and not on 'Label's.
--   All counting is done relative to 0, making this function compatible with
--   relative addressing.
--
--   The pairs in the output are arranged to be compatible with using 'lookup'
--   to determine the address of a label definition.
assignAddresses :: [CodeWord] -> [(CodeWord, Address)]
assignAddresses = snd . mapAccumL assignAddress 0
  where assignAddress a x = (a + codeSize x, (x, a))
        codeSize :: CodeWord -> Address
        codeSize (Label _) = 0
        codeSize (Instr (Jmp _ _))   = 2
        codeSize (Instr (Movi _ _))  = 2
        codeSize (Instr (Movil _ _)) = 2
        codeSize (Instr (IAdd _ _ _)) = 2
        codeSize (Instr (INSub _ _ _)) = 2
        codeSize _ = 1


-- | Assembled code
type Assembled = (Address, (CodeWord, [AssembledWord]))


-- | Assembles a program listing
assembleListing :: [(CodeWord, Address)] -> [Assembled]
assembleListing cws = mapMaybe (assemble cws) cws


-- | Assembles a single line of code
assemble :: [(CodeWord, Address)]
                -> (CodeWord, Address)
                -> Maybe Assembled
assemble _ (Label _, _) = Nothing
assemble cws (cw@(Instr instr), addr) = Just $ assemble' instr
  where encodeRegister = fromIntegral . fromEnum
        encodeRegisterMode (Direct r) = (0, encodeRegister r)
        encodeRegisterMode (Indirect r) = (1, encodeRegister r)
        encodeRegisterMode (IndirectInc r) = (2, encodeRegister r)
        encodeRegisterMode (IndirectDec r) = (3, encodeRegister r)
        -- Instruction encoders
        unaryMath c i r = binaryMath c i R7 r
        binaryMath c i r1 r2 = AssembledInstr $
            0 `shiftL` 10 .|.
            (if c then 1 else 0) `shiftL` 9 .|.
            i `shiftL` 6 .|.
            encodeRegister r1 `shiftL` 3 .|.
            encodeRegister r2
        immMath c f r = AssembledInstr $
            27 `shiftL` 7 .|.
            (if c then 1 else 0) `shiftL` 6 .|.
            f `shiftL` 3 .|.
            encodeRegister r
        unaryLogic i r = binaryLogic i R7 r
        binaryLogic i r1 r2 = AssembledInstr $
            1 `shiftL` 10 .|.
            i `shiftL` 6 .|.
            encodeRegister r1 `shiftL` 3 .|.
            encodeRegister r2
        encodeMov r1 r2 =
            let (p, a) = encodeRegisterMode r1
                (r, b) = encodeRegisterMode r2
            in AssembledInstr $
                  2 `shiftL` 10 .|.
                  p `shiftL` 8 .|.
                  r `shiftL` 6 .|.
                  a `shiftL` 3 .|.
                  b
        -- Math
        assemble' :: Instruction -> (Address, (CodeWord, [AssembledWord]))
        assemble' (Add c r1 r2)  = (addr, (cw, [binaryMath c 0 r1 r2]))
        assemble' (Jmp False lbl) = assemble' (Movil lbl $ Direct R0)
        -- TODO: the address encoding for Jmp requires us to know if
        --       ip is incremented before or after execution.  Will
        --       need to revisit this.
        assemble' (Jmp True lbl) = let i = fromJust $ lookup (Label lbl) cws
                                       i' = i - addr
                                   in (addr, (cw, [ immMath True 0 R0
                                                  , AssembledLiter i'
                                                  ]))
        assemble' (Sub c r1 r2)  = (addr, (cw, [binaryMath c 1 r1 r2]))
        assemble' (Nsub c r1 r2) = (addr, (cw, [binaryMath c 2 r1 r2]))
        assemble' (Dec c r) = (addr, (cw, [unaryMath c 3 r]))
        assemble' (Inc c r) = (addr, (cw, [unaryMath c 4 r]))
        assemble' (Neg c r) = (addr, (cw, [unaryMath c 5 r]))
        assemble' (SLt c r1 r2) = (addr, (cw, [binaryMath c 6 r1 r2]))
        assemble' (ULt c r1 r2) = (addr, (cw, [binaryMath c 7 r1 r2]))
        assemble' (Eq r1 r2) = let eq = AssembledInstr $
                                        50 `shiftL` 6 .|.
                                        encodeRegister r1 `shiftL` 3 .|.
                                        encodeRegister r2
                               in (addr, (cw, [eq]))
        -- Immediate math
        assemble' (IAdd c eil r)  = let i = case eil of
                                                Left i' -> i'
                                                Right lbl -> fromJust $ lookup (Label lbl) cws
                                    in (addr, (cw, [ immMath c 0 r
                                                   , AssembledLiter i
                                                   ]))
        assemble' (INSub c eil r) = let i = case eil of
                                                Left i' -> i'
                                                Right lbl -> fromJust $ lookup (Label lbl) cws
                                    in (addr, (cw, [ immMath c 1 r
                                                   , AssembledLiter i
                                                   ]))
        -- Logic
        assemble' (Not r)       = (addr, (cw, [unaryLogic 0 r]))
        assemble' (Nor r1 r2)   = (addr, (cw, [binaryLogic 1 r1 r2]))
        assemble' (AndN2 r1 r2) = (addr, (cw, [binaryLogic 2 r1 r2]))
        assemble' (Clr r)       = (addr, (cw, [unaryLogic 3 r]))
        assemble' (Nand r1 r2)  = (addr, (cw, [binaryLogic 4 r1 r2]))
        assemble' (CopyN1 r1 r2) = (addr, (cw, [binaryLogic 5 r1 r2]))
        assemble' (Xor r1 r2)   = (addr, (cw, [binaryLogic 6 r1 r2]))
        assemble' (AndN1 r1 r2) = (addr, (cw, [binaryLogic 7 r1 r2]))
        assemble' (OrN2 r1 r2)  = (addr, (cw, [binaryLogic 8 r1 r2]))
        assemble' (Xnor r1 r2)  = (addr, (cw, [binaryLogic 9 r1 r2]))
        assemble' (Copy r1 r2)  = (addr, (cw, [binaryLogic 10 r1 r2]))
        assemble' (And r1 r2)   = (addr, (cw, [binaryLogic 11 r1 r2]))
        assemble' (Set r)       = (addr, (cw, [unaryLogic 12 r]))
        assemble' (OrN1 r1 r2)  = (addr, (cw, [binaryLogic 13 r1 r2]))
        assemble' (Or r1 r2)    = (addr, (cw, [binaryLogic 14 r1 r2]))
        assemble' (Tst r)       = (addr, (cw, [unaryLogic 15 r]))
        -- Loads
        assemble' (Mov r1 r2) = (addr, (cw, [encodeMov r1 r2]))
        assemble' (Movi i r) = (addr, (cw, [ encodeMov (IndirectInc R0) r
                                           , AssembledLiter i
                                           ]))
        assemble' (Movil lbl r) = let i = fromJust $ lookup (Label lbl) cws
                                  in (addr, (cw, [ encodeMov (IndirectInc R0) r
                                                 , AssembledLiter i
                                                 ]))
        -- Rotations
        assemble' (Rot i r) = let ror = AssembledInstr $
                                        26 `shiftL` 7 .|.
                                        i `shiftL` 3 .|.
                                        encodeRegister r
                              in (addr, (cw, [ror]))
        -- Literals
        assemble' (Liti i) = (addr, (cw, [AssembledLiter i]))
        assemble' (Litl s) = let i = fromJust $ lookup (Label s) cws
                             in (addr, (cw, [AssembledLiter i]))



