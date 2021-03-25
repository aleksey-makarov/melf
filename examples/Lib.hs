module Lib (lib) where

import Control.Monad.Catch
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Headers

import Asm

lib :: MonadCatch m => m Elf'
lib  =  do

    txt <- getCode $ do
        -- adc "l"
        label "l"
        adc "l"
        label "m"
        adc "m"

    return $ SELFCLASS64 :&: ElfList
        [ ElfHeader
            { ehData       = ELFDATA2LSB
            , ehOSABI      = ELFOSABI_LINUX
            , ehABIVersion = 1
            , ehType       = ET_REL
            , ehMachine    = EM_AARCH64
            , ehEntry      = 0
            , ehFlags      = 0
            }
        , ElfSegmentTable
        , ElfSectionTable
        , ElfSegment
            { epType     = PT_LOAD
            , epFlags    = PF_R
            , epVirtAddr = 0
            , epPhysAddr = 0
            , epMemSize  = 0
            , epAlign    = 0x100
            , epData     =
                [ ElfSection
                    { esName      = ".some_other_section"
                    , esType      = SHT_PROGBITS
                    , esFlags     = SHF_EXECINSTR
                    , esAddr      = 0
                    , esAddrAlign = 0
                    , esEntSize   = 0
                    , esN         = 1
                    , esLink      = 0
                    , esData      = ElfSectionData $ BSLC.pack "Hello World!"
                    }
                ]
            }
        , ElfSection
            { esName      = ".some_section"
            , esType      = SHT_PROGBITS
            , esFlags     = SHF_EXECINSTR
            , esAddr      = 0
            , esAddrAlign = 0
            , esEntSize   = 0
            , esN         = 3
            , esLink      = 0
            , esData      = ElfSectionData txt
            }
        , ElfSection
            { esName      = ".some_string_section"
            , esType      = SHT_SYMTAB
            , esFlags     = 0
            , esAddr      = 0
            , esAddrAlign = 0
            , esEntSize   = 0
            , esN         = 2
            , esLink      = 0
            , esData      = ElfSectionDataStringTable
            }
        ]
