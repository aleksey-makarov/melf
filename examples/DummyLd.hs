{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DummyLd (dummyLd) where

import Control.Monad.Catch
import Data.Bits
import Data.ByteString.Lazy as BSL
import Data.Word
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Control.Exception.ChainedException

addr :: Word64
addr = 0x400000

mkExe :: MonadThrow m => BSL.ByteString -> m (ElfList 'ELFCLASS64)
mkExe txt = return $ ElfList [ segment ]
    where
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

dummyLd' :: MonadThrow m => ElfList 'ELFCLASS64 -> m (ElfList 'ELFCLASS64)
dummyLd' (ElfList es) = do

    txtSection <- elfFindSectionByName es ".text"

    case txtSection of
        ElfSection{esData = ElfSectionData textData} -> mkExe textData
        _ -> $chainedError "could not find correct \".text\" section"

-- | It places the content of ".text" section of the input ELF into the
--   loadable segment of the resulting ELF.
--   It could work if there are no relocations or references to external symbols
dummyLd :: MonadThrow m => Elf -> m Elf
dummyLd (SELFCLASS32 :&: _)  = $chainedError "AArch64 arch object expected"
dummyLd (SELFCLASS64 :&: es) = (SELFCLASS64 :&:) <$> dummyLd' es
