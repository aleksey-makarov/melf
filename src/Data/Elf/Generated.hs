{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Elf.Generated where

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits

import Data.Elf.TH

-- FIXME: these should go to a separate module
newtype Be a = Be { fromBe :: a } deriving Eq
newtype Le a = Le { fromLe :: a } deriving Eq

instance Binary (Be Word16) where
    put = putWord16be . fromBe
    get = Be <$> getWord16be

instance Binary (Le Word16) where
    put = putWord16le . fromLe
    get = Le <$> getWord16le

instance Binary (Be Word32) where
    put = putWord32be . fromBe
    get = Be <$> getWord32be

instance Binary (Le Word32) where
    put = putWord32le . fromLe
    get = Le <$> getWord32le

instance Binary (Be Word64) where
    put = putWord64be . fromBe
    get = Be <$> getWord64be

instance Binary (Le Word64) where
    put = putWord64le . fromLe
    get = Le <$> getWord64le

$(mkDeclarations BaseWord8 "ElfOSABI" "ELFOSABI" "ELFOSABI_EXT"
    [ ("_SYSV",         0) -- No extensions or unspecified
    , ("_HPUX",         1) -- Hewlett-Packard HP-UX
    , ("_NETBSD",       2) -- NetBSD
    , ("_LINUX",        3) -- Linux
    , ("_SOLARIS",      6) -- Sun Solaris
    , ("_AIX",          7) -- AIX
    , ("_IRIX",         8) -- IRIX
    , ("_FREEBSD",      9) -- FreeBSD
    , ("_TRU64",       10) -- Compaq TRU64 UNIX
    , ("_MODESTO",     11) -- Novell Modesto
    , ("_OPENBSD",     12) -- Open BSD
    , ("_OPENVMS",     13) -- Open VMS
    , ("_NSK",         14) -- Hewlett-Packard Non-Stop Kernel
    , ("_AROS",        15) -- Amiga Research OS
    , ("_ARM",         97) -- ARM
    , ("_STANDALONE", 255) -- Standalone (embedded) application
    ])

$(mkDeclarations BaseWord16 "ElfType" "ET" "ET_EXT"
    [ ("_NONE", 0) -- Unspecified type
    , ("_REL",  1) -- Relocatable object file
    , ("_EXEC", 2) -- Executable object file
    , ("_DYN",  3) -- Shared object file
    , ("_CORE", 4) -- Core dump object file
    ])

$(mkDeclarations BaseWord16 "ElfMachine" "EM" "EM_EXT"
    [ ("_NONE",         0) -- No machine
    , ("_M32",          1) -- AT&T WE 32100
    , ("_SPARC",        2) -- SPARC
    , ("_386",          3) -- Intel 80386
    , ("_68K",          4) -- Motorola 68000
    , ("_88K",          5) -- Motorola 88000
    , ("_486",          6) -- Intel i486 (DO NOT USE THIS ONE)
    , ("_860",          7) -- Intel 80860
    , ("_MIPS",         8) -- MIPS I Architecture
    , ("_S370",         9) -- IBM System/370 Processor
    , ("_MIPS_RS3_LE", 10) -- MIPS RS3000 Little-endian
    , ("_SPARC64",     11) -- SPARC 64-bit
    , ("_PARISC",      15) -- Hewlett-Packard PA-RISC
    , ("_VPP500",      17) -- Fujitsu VPP500
    , ("_SPARC32PLUS", 18) -- Enhanced instruction set SPARC
    , ("_960",         19) -- Intel 80960
    , ("_PPC",         20) -- PowerPC
    , ("_PPC64",       21) -- 64-bit PowerPC
    , ("_S390",        22) -- IBM System/390 Processor
    , ("_SPU",         23) -- Cell SPU
    , ("_V800",        36) -- NEC V800
    , ("_FR20",        37) -- Fujitsu FR20
    , ("_RH32",        38) -- TRW RH-32
    , ("_RCE",         39) -- Motorola RCE
    , ("_ARM",         40) -- Advanced RISC Machines ARM
    , ("_ALPHA",       41) -- Digital Alpha
    , ("_SH",          42) -- Hitachi SH
    , ("_SPARCV9",     43) -- SPARC Version 9
    , ("_TRICORE",     44) -- Siemens TriCore embedded processor
    , ("_ARC",         45) -- Argonaut RISC Core, Argonaut Technologies Inc.
    , ("_H8_300",      46) -- Hitachi H8/300
    , ("_H8_300H",     47) -- Hitachi H8/300H
    , ("_H8S",         48) -- Hitachi H8S
    , ("_H8_500",      49) -- Hitachi H8/500
    , ("_IA_64",       50) -- Intel IA-64 processor architecture
    , ("_MIPS_X",      51) -- Stanford MIPS-X
    , ("_COLDFIRE",    52) -- Motorola ColdFire
    , ("_68HC12",      53) -- Motorola M68HC12
    , ("_MMA",         54) -- Fujitsu MMA Multimedia Accelerator
    , ("_PCP",         55) -- Siemens PCP
    , ("_NCPU",        56) -- Sony nCPU embedded RISC processor
    , ("_NDR1",        57) -- Denso NDR1 microprocessor
    , ("_STARCORE",    58) -- Motorola Star*Core processor
    , ("_ME16",        59) -- Toyota ME16 processor
    , ("_ST100",       60) -- STMicroelectronics ST100 processor
    , ("_TINYJ",       61) -- Advanced Logic Corp. TinyJ embedded processor family
    , ("_X86_64",      62) -- AMD x86-64 architecture
    , ("_PDSP",        63) -- Sony DSP Processor
    , ("_FX66",        66) -- Siemens FX66 microcontroller
    , ("_ST9PLUS",     67) -- STMicroelectronics ST9+ 8/16 bit microcontroller
    , ("_ST7",         68) -- STMicroelectronics ST7 8-bit microcontroller
    , ("_68HC16",      69) -- Motorola MC68HC16 Microcontroller
    , ("_68HC11",      70) -- Motorola MC68HC11 Microcontroller
    , ("_68HC08",      71) -- Motorola MC68HC08 Microcontroller
    , ("_68HC05",      72) -- Motorola MC68HC05 Microcontroller
    , ("_SVX",         73) -- Silicon Graphics SVx
    , ("_ST19",        74) -- STMicroelectronics ST19 8-bit microcontroller
    , ("_VAX",         75) -- Digital VAX
    , ("_CRIS",        76) -- Axis Communications 32-bit embedded processor
    , ("_JAVELIN",     77) -- Infineon Technologies 32-bit embedded processor
    , ("_FIREPATH",    78) -- Element 14 64-bit DSP Processor
    , ("_ZSP",         79) -- LSI Logic 16-bit DSP Processor
    , ("_MMIX",        80) -- Donald Knuth's educational 64-bit processor
    , ("_HUANY",       81) -- Harvard University machine-independent object files
    , ("_PRISM",       82) -- SiTera Prism
    , ("_AVR",         83) -- Atmel AVR 8-bit microcontroller
    , ("_FR30",        84) -- Fujitsu FR30
    , ("_D10V",        85) -- Mitsubishi D10V
    , ("_D30V",        86) -- Mitsubishi D30V
    , ("_V850",        87) -- NEC v850
    , ("_M32R",        88) -- Mitsubishi M32R
    , ("_MN10300",     89) -- Matsushita MN10300
    , ("_MN10200",     90) -- Matsushita MN10200
    , ("_PJ",          91) -- picoJava
    , ("_OPENRISC",    92) -- OpenRISC 32-bit embedded processor
    , ("_ARC_A5",      93) -- ARC Cores Tangent-A5
    , ("_XTENSA",      94) -- Tensilica Xtensa Architecture
    , ("_VIDEOCORE",   95) -- Alphamosaic VideoCore processor
    , ("_TMM_GPP",     96) -- Thompson Multimedia General Purpose Processor
    , ("_NS32K",       97) -- National Semiconductor 32000 series
    , ("_TPC",         98) -- Tenor Network TPC processor
    , ("_SNP1K",       99) -- Trebia SNP 1000 processor
    , ("_ST200",      100) -- STMicroelectronics (www.st.com) ST200 microcontroller
    , ("_IP2K",       101) -- Ubicom IP2xxx microcontroller family
    , ("_MAX",        102) -- MAX Processor
    , ("_CR",         103) -- National Semiconductor CompactRISC microprocessor
    , ("_F2MC16",     104) -- Fujitsu F2MC16
    , ("_MSP430",     105) -- Texas Instruments embedded microcontroller msp430
    , ("_BLACKFIN",   106) -- Analog Devices Blackfin (DSP) processor
    , ("_SE_C33",     107) -- S1C33 Family of Seiko Epson processors
    , ("_SEP",        108) -- Sharp embedded microprocessor
    , ("_ARCA",       109) -- Arca RISC Microprocessor
    , ("_UNICORE",    110) -- Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University
    ])

$(mkDeclarations BaseWord32 "ElfSectionType" "SHT" "SHT_EXT"
    [ ("_NULL",     0) -- Identifies an empty section header.
    , ("_PROGBITS", 1) -- Contains information defined by the program
    , ("_SYMTAB",   2) -- Contains a linker symbol table
    , ("_STRTAB",   3) -- Contains a string table
    , ("_RELA",     4) -- Contains "Rela" type relocation entries
    , ("_HASH",     5) -- Contains a symbol hash table
    , ("_DYNAMIC",  6) -- Contains dynamic linking tables
    , ("_NOTE",     7) -- Contains note information
    , ("_NOBITS",   8) -- Contains uninitialized space; does not occupy any space in the file
    , ("_REL",      9) -- Contains "Rel" type relocation entries
    , ("_SHLIB",   10) -- Reserved
    , ("_DYNSYM",  11) -- Contains a dynamic loader symbol table
    ])

$(mkDeclarations BaseWord32 "ElfSegmentType" "PT" "PT_EXT"
    [ ("_NULL",    0) -- Unused entry
    , ("_LOAD",    1) -- Loadable segment
    , ("_DYNAMIC", 2) -- Dynamic linking tables
    , ("_INTERP",  3) -- Program interpreter path name
    , ("_NOTE",    4) -- Note section
    , ("_SHLIB",   5) -- Reserved
    , ("_PHDR",    6) -- Program header table
    ])

$(mkDeclarations BaseWord64 "ElfSectionFlag" "SHF" "SHF_EXT"
    [ ("_WRITE",     (1 `shiftL` 0)) -- Section contains writable data
    , ("_ALLOC",     (1 `shiftL` 1)) -- Section is allocated in memory image of program
    , ("_EXECINSTR", (1 `shiftL` 2)) -- Section contains executable instructions
    ])

$(mkDeclarations BaseWord32 "ElfSegmentFlag" "PF" "PF_EXT"
    [ ("_X", (1 `shiftL` 0)) -- Execute permission
    , ("_W", (1 `shiftL` 1)) -- Write permission
    , ("_R", (1 `shiftL` 2)) -- Read permission
    ])

$(mkDeclarations BaseWord8 "ElfSymbolType" "STT" "STT_EXT"
    [ ("_NoType",  0)
    , ("_Object",  1)
    , ("_Func",    2)
    , ("_Section", 3)
    , ("_File",    4)
    , ("_Common",  5)
    , ("_TLS",     6)
    , ("_LoOS",   10)
    , ("_HiOS",   12)
    , ("_LoProc", 13)
    , ("_HiProc", 15)
    ])

$(mkDeclarations BaseWord8 "ElfSymbolBinding" "STB" "STB_EXT"
    [ ("_Local",   0)
    , ("_Global",  1)
    , ("_Weak",    2)
    , ("_LoOS",   10)
    , ("_HiOS",   12)
    , ("_LoProc", 13)
    , ("_HiProc", 15)
    ])

$(mkDeclarations BaseWord16 "ElfSectionIndex" "SHN" "SHN_EXT"
    [ ("_Undef",       0)
    , ("_LoProc", 0xFF00)
    , ("_HiProc", 0xFF1F)
    , ("_LoOS",   0xFF20)
    , ("_HiOS",   0xFF3F)
    , ("_Abs",    0xFFF1)
    , ("_Common", 0xFFF2)
    ])
