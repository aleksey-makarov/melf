module SysCall (syscall) where

import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Singletons.Sigma

import Data.Elf
import Data.Elf.Headers

syscall :: Elf'
syscall  =  SELFCLASS64 :&: ElfList
        [ ElfSegment
            { epType     = PT_LOAD
            , epFlags    = PF_X .|. PF_R
            , epVirtAddr = 0x400000
            , epPhysAddr = 0x400000
            , epMemSize  = 0x84
            , epAlign    = 0x10000
            , epData     =
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
                    { erData = BSL.pack [0x40, 0x05, 0x80, 0xd2, 0xa8, 0x0b, 0x80, 0xd2, 0x01, 0x00, 0x00, 0xd4]
                    }
                ]
            }
        ]
