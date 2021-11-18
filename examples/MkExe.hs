module MkExe (mkExe) where

import Control.Monad.Catch
import Control.Monad.State
import Data.Bits
import Data.Word
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

import AsmAArch64

addr :: Word64
addr = 0x400000

mkExe :: MonadCatch m => StateT CodeState m () -> m Elf
mkExe m = do
    (txt, _) <- assemble 1 m
    let
        segment = ElfSegment
            { epType       = PT_LOAD
            , epFlags      = PF_X .|. PF_R
            , epVirtAddr   = addr
            , epPhysAddr   = addr
            , epAddMemSize = 0
            , epAlign      = 0x10000
            , epData       =
                [ ElfHeader
                    { ehData       = ELFDATA2LSB
                    , ehOSABI      = ELFOSABI_SYSV
                    , ehABIVersion = 0
                    , ehType       = ET_EXEC
                    , ehMachine    = EM_AARCH64
                    , ehEntry      = addr + headerSize ELFCLASS64
                    , ehFlags      = 0
                    }
                , ElfRawData
                    { edData = txt
                    }
                , ElfSegmentTable
                ]
            }
    return $ SELFCLASS64 :&: ElfList [ segment ]
