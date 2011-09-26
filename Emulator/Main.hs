-- | Entry point to the emulator and assembler
module Main where


import Control.Monad


import Assembler
import Assembly
import Emulator
import MachineInteger


-- * Entry to the emulator


-- | Run the emulator for the given number of instructions
runEmulator :: FilePath -> Int -> IO ()
runEmulator path rc =
     do e <- assembleFile path
        case e of
            Left pe -> putStrLn $ show pe
            Right asm ->
                 do forM_ asm $ putStrLn . show
                    let state = Machine
                                    { m_memory = concatMap fromAssembled asm
                                    }
                    e' <- runEmulatorT (replicateM_ rc (fetch >>= execute)) state
                    case e' of
                        Left me -> putStrLn $ show me
                        Right (_, s) -> putStrLn $ show s
  where fromAssembled :: Assembled -> [(MachineInteger, MachineInteger)]
        fromAssembled (addr, (_, aw)) = [ MachineInteger $ addr + k | k <- [0 ..] ]
                                            `zip` map awTomi aw
