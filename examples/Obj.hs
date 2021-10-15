module Obj (obj) where

import Data.Bits
-- import Data.Word
import Control.Monad.Catch
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

import Asm

obj :: MonadCatch m => m Elf
obj  =  do

    let
        msg :: ByteString
        msg = BSLC.pack "Hello World!\n"

    txt <- getCode $ do
        mov x0 1                                -- mov x0, #1
        pool msg >>= ldr x1                     -- ldr x1, =msg
        mov x2 $ fromIntegral $ BSLC.length msg -- ldr x2, =len
        mov x8 64                               -- mov x8, #64 // write()
        svc 0                                   -- svc #0
                                                --
        mov x0 0                                -- mov x0, #0
        mov x8 93                               -- mov x8, #93 // exit()
        svc 0                                   -- svc #0
                                                --
                                                -- .ascii "Hello World!\n"

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
            , esN         = 1
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
            , esN         = 2
            , esLink      = 0
            , esInfo      = 0
            , esData      = ElfSectionDataStringTable
            }
        , ElfSectionTable
        ]
