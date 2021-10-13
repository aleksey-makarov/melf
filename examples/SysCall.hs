module SysCall (syscall) where

import Control.Monad.Catch
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

addr :: Word64
addr = 0x400000

syscall :: MonadCatch m => m Elf
syscall = return $ SELFCLASS64 :&: ElfList [ segment ]
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
                    , ehEntry      = 0x400078
                    , ehFlags      = 0
                    }
                , ElfSegmentTable
                , ElfRawData
                    { edData = txt
                    }
                ]
            }
        txt = BSL.pack [
            0x20, 0x00, 0x80, 0xd2,
            0x61, 0x01, 0x00, 0x58,
            0x82, 0x01, 0x00, 0x58,
            0x08, 0x08, 0x80, 0xd2,
            0x01, 0x00, 0x00, 0xd4,
            0x00, 0x00, 0x80, 0xd2,
            0xa8, 0x0b, 0x80, 0xd2,
            0x01, 0x00, 0x00, 0xd4,
            0x48, 0x65, 0x6c, 0x6c,
            0x6f, 0x20, 0x57, 0x6f,
            0x72, 0x6c, 0x64, 0x21,
            0x0a,
            0x00, 0x00, 0x00, 0x98,
            0x00,
            0x40, 0x00,
            0x00, 0x00, 0x00, 0x00,
            0x0d, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00
            ]
