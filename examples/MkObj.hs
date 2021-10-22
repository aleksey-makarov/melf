{-# LANGUAGE DataKinds #-}

module MkObj (mkObj) where

import Prelude as P

import Control.Monad.Catch
import Data.Bits
import Data.ByteString.Lazy as BSL
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

textSecN, shstrtabSecN, strtabSecN, symtabSecN :: ElfSectionIndex
textSecN     = 1
shstrtabSecN = 2
strtabSecN   = 3
symtabSecN   = 4

mkObj :: MonadCatch m => (ElfSectionIndex -> m (BSL.ByteString, [ElfSymbolXX 'ELFCLASS64])) -> m Elf
mkObj assembleF =  do

    (txt, symbolTable) <- assembleF textSecN
    (symbolTableData, stringTableData) <- serializeSymbolTable ELFDATA2LSB symbolTable

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
            , esType      = SHT_STRTAB
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
            , esInfo      = 1
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
