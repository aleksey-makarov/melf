module Obj (obj) where

import Prelude as P

import Data.Bits
-- import Data.Word
import Control.Monad.Catch
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

import Asm

msg :: String
msg = "Hello World!\n"

textSecN, shstrtabSecN, strtabSecN, symtabSecN :: ElfSectionIndex
textSecN     = 1
shstrtabSecN = 2
strtabSecN   = 3
symtabSecN   = 4

obj :: MonadCatch m => m Elf
obj  =  do

    (txt, symbolTable) <- assemble textSecN $ do
        label >>= exportSymbol "_start"      -- _start:
        mov x0 1                             --     mov x0, #1
        ascii msg >>= ldr x1                 --     ldr x1, =msg
        mov x2 $ fromIntegral $ P.length msg --     ldr x2, =len
        mov x8 64                            --     mov x8, #64 // write()
        svc 0                                --     svc #0
                                             --
        mov x0 0                             --     mov x0, #0
        mov x8 93                            --     mov x8, #93 // exit()
        svc 0                                --     svc #0
                                             --
                                             -- .ascii "Hello World!\n"

    (symbolTableData, stringTableData) <- serializeSymbolTable symbolTable

    return $ SELFCLASS64 :&: ElfList
        [ ElfHeader
            { ehData       = ELFDATA2LSB
            , ehOSABI      = ELFOSABI_SYSV
            , ehABIVersion = 0
            , ehType       = ET_REL
            , ehMachine    = EM_AARCH64
            , ehEntry      = 0
            , ehFlags      = 0
            }
        , ElfSection
            { esName      = ".text"
            , esType      = SHT_PROGBITS
            , esFlags     = SHF_EXECINSTR .|. SHF_ALLOC
            , esAddr      = 0
            , esAddrAlign = 8
            , esEntSize   = 0
            , esN         = textSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionData txt
            }
        , ElfSection
            { esName      = ".shstrtab"
            , esType      = SHT_SYMTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 1
            , esEntSize   = 0
            , esN         = shstrtabSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionDataStringTable
            }
        , ElfSection
            { esName      = ".symtab"
            , esType      = SHT_SYMTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 8
            , esEntSize   = symbolTableEntrySize ELFCLASS64
            , esN         = symtabSecN
            , esLink      = fromIntegral strtabSecN
            , esInfo      = 0
            , esData      = ElfSectionData symbolTableData
            }
        , ElfSection
            { esName      = ".strtab"
            , esType      = SHT_STRTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 1
            , esEntSize   = 0
            , esN         = strtabSecN
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionData stringTableData
            }
        , ElfSectionTable
        ]
