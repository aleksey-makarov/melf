-- |
-- Module      : Data.ELF.Headers
-- Description : Parse headers and table entries of ELF files
-- Copyright   : (c) Aleksey Makarov, 2021
-- License     : BSD 3-Clause License
-- Maintainer  : aleksey.makarov@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Parse headers and table entries of ELF files

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE CPP #-}

#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(8,10,0,0)
{-# LANGUAGE StandaloneKindSignatures #-}
#endif
#endif

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Data.Elf.Headers (
    -- * Data definition
      elfMagic
    , ElfClass(..)
    , SElfClass (..)
    , ElfData(..)

    , IsElfClass(..)
    , wordSize
    , withElfClass

    -- * Types of ELF header
    , HeaderXX(..)
    , headerSize
    , Header

    -- * Types of ELF tables

    -- ** Section table
    , SectionXX(..)
    , sectionTableEntrySize

    -- ** Segment table
    , SegmentXX(..)
    , segmentTableEntrySize

    -- ** Sybmol table
    , SymbolXX(..)
    , symbolTableEntrySize

    -- ** Relocation table
    , RelaXX(..)
    , relocationTableAEntrySize

    -- * Parse header and section and segment tables
    , HeadersXX (..)
    , parseHeaders

    -- * Parse/serialize array of data

    -- | BList is an internal newtype for @[a]@ that is an instance of `Data.Binary.Binary`.
    -- When serializing, the @Binary@ instance for BList does not write the length of the array to the stream.
    -- Instead, parser just reads all the stream till the end.

    , parseBList
    , serializeBList

    -- * Misc helpers
    , sectionIsSymbolTable

    ) where

import Control.Monad
import Control.Monad.Catch
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString       as BS
import Data.ByteString.Lazy  as BSL
import Data.Data (Data)
import qualified Data.List as L
import Data.Singletons.Sigma
import Data.Singletons.TH
import Data.Typeable (Typeable)

#if MIN_VERSION_singletons(3,0,0)
import Data.Eq.Singletons
import Text.Show.Singletons
import Data.Bool.Singletons
#endif

import Control.Exception.ChainedException
import Data.BList
import Data.Endian
import Data.Elf.Constants

-- | ELF class.  Tells if ELF defines 32- or 64-bit objects
$(singletons [d|
    data ElfClass
        = ELFCLASS32 -- ^ 32-bit ELF format
        | ELFCLASS64 -- ^ 64-bit ELF format
        deriving (Eq, Show)
    |])

instance Binary ElfClass where
    get = getWord8 >>= getElfClass_
        where
            getElfClass_ 1 = return ELFCLASS32
            getElfClass_ 2 = return ELFCLASS64
            getElfClass_ _ = fail "Invalid ELF class"
    put ELFCLASS32 = putWord8 1
    put ELFCLASS64 = putWord8 2

-- | ELF data. Specifies the endianness of the ELF data
data ElfData
    = ELFDATA2LSB -- ^ Little-endian ELF format
    | ELFDATA2MSB -- ^ Big-endian ELF format
    deriving (Eq, Show)

instance Binary ElfData where
    get = getWord8 >>= getElfData_
        where
            getElfData_ 1 = return ELFDATA2LSB
            getElfData_ 2 = return ELFDATA2MSB
            getElfData_ _ = fail "Invalid ELF data"
    put ELFDATA2LSB = putWord8 1
    put ELFDATA2MSB = putWord8 2

elfSupportedVersion :: Word8
elfSupportedVersion = 1

-- at :: (Integral i) => [a] -> i -> Maybe a
-- at (x : _)  0             = Just x
-- at (_ : xs) n | n > 0     = xs `at` (n - 1)
--               | otherwise = Nothing
-- at _        _             = Nothing

-- nameToString :: Maybe BS.ByteString -> String
-- nameToString bs = maybe "" id $ BSC.unpack <$> bs

-- cut :: BS.ByteString -> Int -> Int -> BS.ByteString
-- cut content offset size = BS.take size $ BS.drop offset content

-- | The first 4 bytes of the ELF file
elfMagic :: Be Word32
elfMagic = Be 0x7f454c46 -- "\DELELF"

verify :: (Binary a, Eq a) => String -> a -> Get ()
verify msg orig = do
    a <- get
    when (orig /= a) $ error ("verification failed: " ++ msg)

-- getTable :: (Binary (Le a), Binary (Be a)) => ElfData -> Word64 -> Word16 -> Word16 -> Get [a]
-- getTable endianness offset entrySize entryNumber = lookAhead $ do
--     skip $ fromIntegral offset
--     getTable' entryNumber
--     where
--         getTable' 0 = return []
--         getTable' n = do
--             a <- isolate (fromIntegral entrySize) $ getEndian endianness
--             (a :) <$> getTable' (n - 1)

getEndian :: (Binary (Le a), Binary (Be a)) => ElfData -> Get a
getEndian ELFDATA2LSB = fromLe <$> get
getEndian ELFDATA2MSB = fromBe <$> get

getBe :: (Binary (Le b), Binary (Be b)) => Get b
getBe = getEndian ELFDATA2MSB

getLe :: (Binary (Le b), Binary (Be b)) => Get b
getLe = getEndian ELFDATA2LSB

putEndian :: (Binary (Le a), Binary (Be a)) => ElfData -> a -> Put
putEndian ELFDATA2LSB = put . Le
putEndian ELFDATA2MSB = put . Be

putBe :: (Binary (Le b), Binary (Be b)) => b -> Put
putBe = putEndian ELFDATA2MSB

putLe :: (Binary (Le b), Binary (Be b)) => b -> Put
putLe = putEndian ELFDATA2LSB

--------------------------------------------------------------------------
-- WordXX
--------------------------------------------------------------------------

-- | @IsElfClass a@ is defined for each constructor of `ElfClass`.
--   It defines @WordXX a@, which is `Word32` for `ELFCLASS32` and `Word64` for `ELFCLASS64`.
class ( SingI c
      , Typeable c
      , Typeable (WordXX c)
      , Data (WordXX c)
      , Show (WordXX c)
      , Read (WordXX c)
      , Eq (WordXX c)
      , Ord (WordXX c)
      , Bounded (WordXX c)
      , Enum (WordXX c)
      , Num (WordXX c)
      , Integral (WordXX c)
      , Real (WordXX c)
      , Bits (WordXX c)
      , FiniteBits (WordXX c)
      , Binary (Be (WordXX c))
      , Binary (Le (WordXX c))
      ) => IsElfClass (c :: ElfClass) where
    type WordXX c = r | r -> c

instance IsElfClass 'ELFCLASS32 where
    type WordXX 'ELFCLASS32 = Word32

instance IsElfClass 'ELFCLASS64 where
    type WordXX 'ELFCLASS64 = Word64

--------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------

-- | Parsed ELF header
data HeaderXX c =
    HeaderXX
        { hData       :: ElfData    -- ^ Data encoding (big- or little-endian)
        , hOSABI      :: ElfOSABI   -- ^ OS/ABI identification
        , hABIVersion :: Word8      -- ^ ABI version
        , hType       :: ElfType    -- ^ Object file type
        , hMachine    :: ElfMachine -- ^ Machine type
        , hEntry      :: WordXX c   -- ^ Entry point address
        , hPhOff      :: WordXX c   -- ^ Program header offset
        , hShOff      :: WordXX c   -- ^ Section header offset
        , hFlags      :: Word32     -- ^ Processor-specific flags
        , hPhEntSize  :: Word16     -- ^ Size of program header entry
        , hPhNum      :: Word16     -- ^ Number of program header entries
        , hShEntSize  :: Word16     -- ^ Size of section header entry
        , hShNum      :: Word16     -- ^ Number of section header entries
        , hShStrNdx   :: ElfSectionIndex -- ^ Section name string table index
        }

-- | Sigma type where `ElfClass` defines the type of `HeaderXX`
type Header = Sigma ElfClass (TyCon1 HeaderXX)

-- | Size of ELF header.
headerSize :: Num a => ElfClass -> a
headerSize ELFCLASS64 = 64
headerSize ELFCLASS32 = 52

-- | Size of section table entry.
sectionTableEntrySize :: Num a => ElfClass -> a
sectionTableEntrySize ELFCLASS64 = 64
sectionTableEntrySize ELFCLASS32 = 40

-- | Size of segment table entry.
segmentTableEntrySize :: Num a => ElfClass -> a
segmentTableEntrySize ELFCLASS64 = 56
segmentTableEntrySize ELFCLASS32 = 32

-- | Size of symbol table entry.
symbolTableEntrySize :: Num a => ElfClass -> a
symbolTableEntrySize ELFCLASS64 = 24
symbolTableEntrySize ELFCLASS32 = 16

-- | Size of @WordXX a@ in bytes.
wordSize :: Num a => ElfClass -> a
wordSize ELFCLASS64 = 8
wordSize ELFCLASS32 = 4

-- | Convenience function for creating a context with an implicit ElfClass available.
withElfClass :: Sing c -> (IsElfClass c => a) -> a
withElfClass SELFCLASS64 x = x
withElfClass SELFCLASS32 x = x

getHeader' :: IsElfClass c => Sing c -> Get Header
getHeader' classS = do

    hData <- get
    verify "version1" elfSupportedVersion
    hOSABI <- get
    hABIVersion <- get
    skip 7

    let
        getE :: (Binary (Le b), Binary (Be b)) => Get b
        getE = getEndian hData

    hType <- getE
    hMachine <- getE

    (hVersion2 :: Word32) <- getE
    when (hVersion2 /= 1) $ error "verification failed: version2"

    hEntry <- getE
    hPhOff <- getE
    hShOff <- getE

    hFlags <- getE
    (hSize :: Word16) <- getE
    when (hSize /= headerSize (fromSing classS)) $ error "incorrect size of elf header"
    hPhEntSize <- getE
    hPhNum <- getE
    hShEntSize <- getE
    hShNum <- getE
    hShStrNdx <- getE

    return $ classS :&: HeaderXX{..}

getHeader :: Get Header
getHeader = do
    verify "magic" elfMagic
    hClass <- get
    let
        f2 :: forall (c :: ElfClass) . Sing c -> Get Header
        f2 x = withElfClass x (getHeader' x)

    withSomeSing hClass f2

putHeader :: Header -> Put
putHeader (classS :&: HeaderXX{..}) = withElfClass classS do

    put elfMagic
    put $ fromSing classS
    put hData
    put elfSupportedVersion
    put hOSABI
    put hABIVersion

    putByteString $ BS.replicate 7 0

    let
        putE :: (Binary (Le b), Binary (Be b)) => b -> Put
        putE = putEndian hData

    putE hType
    putE hMachine
    putE (1 :: Word32)
    putE hEntry
    putE hPhOff
    putE hShOff
    putE hFlags
    putE (headerSize $ fromSing classS :: Word16)
    putE hPhEntSize
    putE hPhNum
    putE hShEntSize
    putE hShNum
    putE hShStrNdx

instance Binary Header where
    put = putHeader
    get = getHeader

--------------------------------------------------------------------------
-- Section
--------------------------------------------------------------------------

-- | Parsed ELF section table entry
data SectionXX c =
    SectionXX
        { sName      :: Word32         -- ^ Section name
        , sType      :: ElfSectionType -- ^ Section type
        , sFlags     :: WordXX c       -- ^ Section attributes
        , sAddr      :: WordXX c       -- ^ Virtual address in memory
        , sOffset    :: WordXX c       -- ^ Offset in file
        , sSize      :: WordXX c       -- ^ Size of section
        , sLink      :: Word32         -- ^ Link to other section
        , sInfo      :: Word32         -- ^ Miscellaneous information
        , sAddrAlign :: WordXX c       -- ^ Address alignment boundary
        , sEntSize   :: WordXX c       -- ^ Size of entries, if section has table
        }

getSection ::                               IsElfClass c =>
    (forall b . (Binary (Le b), Binary (Be b)) => Get b) -> Get (SectionXX c)
getSection getE = do

    sName      <- getE
    sType      <- getE
    sFlags     <- getE
    sAddr      <- getE
    sOffset    <- getE
    sSize      <- getE
    sLink      <- getE
    sInfo      <- getE
    sAddrAlign <- getE
    sEntSize   <- getE

    return SectionXX {..}

putSection ::                                  IsElfClass c =>
    (forall b . (Binary (Le b), Binary (Be b)) => b -> Put) ->
                                                SectionXX c -> Put
putSection putE (SectionXX{..}) = do

    putE sName
    putE sType
    putE sFlags
    putE sAddr
    putE sOffset
    putE sSize
    putE sLink
    putE sInfo
    putE sAddrAlign
    putE sEntSize

instance forall (a :: ElfClass) . SingI a => Binary (Be (SectionXX a)) where
    put = withElfClass (sing @a) (putSection putBe) . fromBe
    get = Be <$> withElfClass (sing @a) (getSection getBe)

instance forall (a :: ElfClass) . SingI a => Binary (Le (SectionXX a)) where
    put = withElfClass (sing @a) (putSection putLe) . fromLe
    get = Le <$> withElfClass (sing @a) (getSection getLe)

--------------------------------------------------------------------------
-- Segment
--------------------------------------------------------------------------

-- | Parsed ELF segment table entry
data SegmentXX c =
    SegmentXX
        { pType     :: ElfSegmentType -- ^ Type of segment
        , pFlags    :: ElfSegmentFlag -- ^ Segment attributes
        , pOffset   :: WordXX c       -- ^ Offset in file
        , pVirtAddr :: WordXX c       -- ^ Virtual address in memory
        , pPhysAddr :: WordXX c       -- ^ Physical address
        , pFileSize :: WordXX c       -- ^ Size of segment in file
        , pMemSize  :: WordXX c       -- ^ Size of segment in memory
        , pAlign    :: WordXX c       -- ^ Alignment of segment
        }

getSegment ::            forall (c :: ElfClass) . Sing c ->
    (forall b . (Binary (Le b), Binary (Be b)) => Get b) -> Get (SegmentXX c)
getSegment SELFCLASS64 getE = do

    pType     <- getE
    pFlags    <- getE
    pOffset   <- getE
    pVirtAddr <- getE
    pPhysAddr <- getE
    pFileSize <- getE
    pMemSize  <- getE
    pAlign    <- getE

    return SegmentXX{..}

getSegment SELFCLASS32 getE = do

    pType     <- getE
    pOffset   <- getE
    pVirtAddr <- getE
    pPhysAddr <- getE
    pFileSize <- getE
    pMemSize  <- getE
    pFlags    <- getE
    pAlign    <- getE

    return SegmentXX{..}

putSegment ::               forall (c :: ElfClass) . Sing c ->
    (forall b . (Binary (Le b), Binary (Be b)) => b -> Put) ->
                                                SegmentXX c -> Put
putSegment SELFCLASS64 putE (SegmentXX{..}) = do

    putE pType
    putE pFlags
    putE pOffset
    putE pVirtAddr
    putE pPhysAddr
    putE pFileSize
    putE pMemSize
    putE pAlign

putSegment SELFCLASS32 putE (SegmentXX{..}) = do

    putE pType
    putE pOffset
    putE pVirtAddr
    putE pPhysAddr
    putE pFileSize
    putE pMemSize
    putE pFlags
    putE pAlign


instance forall (a :: ElfClass) . SingI a => Binary (Be (SegmentXX a)) where
    put = putSegment sing putBe . fromBe
    get = Be <$> getSegment sing getBe

instance forall (a :: ElfClass) . SingI a => Binary (Le (SegmentXX a)) where
    put = putSegment sing putLe . fromLe
    get = Le <$> getSegment sing getLe

--------------------------------------------------------------------------
-- Symbol table entry
--------------------------------------------------------------------------

-- | Test if the section with such integer value of section type field (`sType`)
--   contains symbol table
sectionIsSymbolTable :: ElfSectionType -> Bool
sectionIsSymbolTable sType  = sType `L.elem` [SHT_SYMTAB, SHT_DYNSYM]

-- | Parsed ELF symbol table entry
data SymbolXX c =
    SymbolXX
        { stName  :: Word32          -- ^ Symbol name
        , stInfo  :: Word8           -- ^ Type and Binding attributes
        , stOther :: Word8           -- ^ Reserved
        , stShNdx :: ElfSectionIndex -- ^ Section table index
        , stValue :: WordXX c        -- ^ Symbol value
        , stSize  :: WordXX c        -- ^ Size of object
        }

getSymbolTableEntry ::    forall (c :: ElfClass) . Sing c ->
     (forall b . (Binary (Le b), Binary (Be b)) => Get b) -> Get (SymbolXX c)
getSymbolTableEntry SELFCLASS64 getE = do

    stName  <- getE
    stInfo  <- get
    stOther <- get
    stShNdx <- getE
    stValue <- getE
    stSize  <- getE

    return SymbolXX{..}

getSymbolTableEntry SELFCLASS32 getE = do

    stName  <- getE
    stValue <- getE
    stSize  <- getE
    stInfo  <- get
    stOther <- get
    stShNdx <- getE

    return SymbolXX{..}

putSymbolTableEntry ::      forall (c :: ElfClass) . Sing c ->
    (forall b . (Binary (Le b), Binary (Be b)) => b -> Put) ->
                                       SymbolXX c -> Put
putSymbolTableEntry SELFCLASS64 putE (SymbolXX{..}) = do

    putE stName
    put  stInfo
    put  stOther
    putE stShNdx
    putE stValue
    putE stSize

putSymbolTableEntry SELFCLASS32 putE (SymbolXX{..}) = do

    putE stName
    putE stValue
    putE stSize
    put  stInfo
    put  stOther
    putE stShNdx

instance forall (a :: ElfClass) . SingI a => Binary (Be (SymbolXX a)) where
    put = putSymbolTableEntry sing putBe . fromBe
    get = Be <$> getSymbolTableEntry sing getBe

instance forall (a :: ElfClass) . SingI a => Binary (Le (SymbolXX a)) where
    put = putSymbolTableEntry sing putLe . fromLe
    get = Le <$> getSymbolTableEntry sing getLe

--------------------------------------------------------------------------
-- relocation table entry
--------------------------------------------------------------------------

-- | Parsed relocation table entry (@ElfXX_Rela@)
data RelaXX c =
    RelaXX
        { relaOffset :: WordXX c -- ^ Address of reference
        , relaSym    :: Word32   -- ^ Symbol table index
        , relaType   :: Word32   -- ^ Relocation type
        , relaAddend :: WordXX c -- ^ Constant part of expression
        }

relaSym32 :: Word32 -> Word32
relaSym32 v = v `shiftR` 8

relaType32 :: Word32 -> Word32
relaType32 v = fromIntegral $ v .&. 0xff

relaSym64 :: Word64 -> Word32
relaSym64 v = fromIntegral $ v `shiftR` 32

relaType64 :: Word64 -> Word32
relaType64 v = fromIntegral $ v .&. 0xffffffff

relaInfo32 :: Word32 -> Word32 -> Word32
relaInfo32 s t = (t .&. 0xff) .|. (s `shiftL` 8)

relaInfo64 :: Word32 -> Word32 -> Word64
relaInfo64 s t = fromIntegral t .|. (fromIntegral s `shiftL` 32)

getRelocationTableAEntry ::      forall c . IsElfClass c =>
    (forall b . (Binary (Le b), Binary (Be b)) => Get b) -> Get (RelaXX c)
getRelocationTableAEntry getE = do
    relaOffset <- getE
    (relaSym, relaType) <- case sing @c of
        SELFCLASS64 -> (\x -> (relaSym64 x, relaType64 x)) <$> getE
        SELFCLASS32 -> (\x -> (relaSym32 x, relaType32 x)) <$> getE
    relaAddend <- getE
    return RelaXX{..}

putRelocationTableAEntry ::         forall c . IsElfClass c =>
    (forall b . (Binary (Le b), Binary (Be b)) => b -> Put) ->
                                  RelaXX c -> Put
putRelocationTableAEntry putE (RelaXX{..}) = do
    putE relaOffset
    (case sing @c of
        SELFCLASS64 -> putE $ relaInfo64 relaSym relaType
        SELFCLASS32 -> putE $ relaInfo32 relaSym relaType) :: Put
    putE relaAddend

instance forall (a :: ElfClass) . SingI a => Binary (Be (RelaXX a)) where
    put = withElfClass (sing @a) (putRelocationTableAEntry putBe) . fromBe
    get = Be <$> withElfClass (sing @a) (getRelocationTableAEntry getBe)

instance forall (a :: ElfClass) . SingI a => Binary (Le (RelaXX a)) where
    put = withElfClass (sing @a) (putRelocationTableAEntry putLe) . fromLe
    get = Le <$> withElfClass (sing @a) (getRelocationTableAEntry getLe)

-- | Size of @RelaXX a@ in bytes.
relocationTableAEntrySize :: forall a . IsElfClass a => WordXX a
relocationTableAEntrySize = fromIntegral $ BSL.length $ encode $ Le $ RelaXX @a 0 0 0 0

--------------------------------------------------------------------------
-- parseHeaders
--------------------------------------------------------------------------

elfDecodeOrFail' :: (Binary a, MonadThrow m) => BSL.ByteString -> m (ByteOffset, a)
elfDecodeOrFail' bs = case decodeOrFail bs of
    Left (_, off, err) -> $chainedError $ err ++ " @" ++ show off
    Right (_, off, a) -> return (off, a)

elfDecodeOrFail :: (Binary a, MonadThrow m) => BSL.ByteString -> m a
elfDecodeOrFail bs = snd <$> elfDecodeOrFail' bs

elfDecodeAllOrFail :: (Binary a, MonadThrow m) => BSL.ByteString -> m a
elfDecodeAllOrFail bs = do
    (off, a) <- elfDecodeOrFail' bs
    if off == BSL.length bs then return a else $chainedError $ "leftover != 0 @" ++ show off

-- | Parse an array
parseBList :: (MonadThrow m, Binary (Le a), Binary (Be a))
           => ElfData        -- ^ Tells if parser should expect big or little endian data
           -> BSL.ByteString -- ^ Data for parsing
           -> m [a]
parseBList d bs = case d of
    ELFDATA2LSB -> fromBList . fromLe <$> elfDecodeAllOrFail bs
    ELFDATA2MSB -> fromBList . fromBe <$> elfDecodeAllOrFail bs

-- | Serialize an array
serializeBList :: (Binary (Le a), Binary (Be a))
               => ElfData -- ^ Tells if serializer should tread the data as bit or little endian
               -> [a]     -- ^ The array to serialize
               -> BSL.ByteString
serializeBList d as = case d of
    ELFDATA2LSB -> encode $ Le $ BList as
    ELFDATA2MSB -> encode $ Be $ BList as

-- FIXME: how to get rid of this? (Can we use some combinators for Sigma)
-- | The type that helps to make the sigma type of the result
--   of the `parseHeaders` function
newtype HeadersXX a = HeadersXX (HeaderXX a, [SectionXX a], [SegmentXX a])

parseHeaders' :: (IsElfClass a, MonadThrow m) => HeaderXX a -> BSL.ByteString -> m (Sigma ElfClass (TyCon1 HeadersXX))
parseHeaders' hxx@HeaderXX{..} bs =
    let
        takeLen off len = BSL.take (fromIntegral len) $ BSL.drop (fromIntegral off) bs
        bsSections = takeLen hShOff (hShEntSize * hShNum)
        bsSegments = takeLen hPhOff (hPhEntSize * hPhNum)
    in do
        ss <- parseBList hData bsSections
        ps <- parseBList hData bsSegments
        return $ sing :&: HeadersXX (hxx, ss, ps)

-- | Parse ELF file and produce header and section and segment tables
parseHeaders :: MonadThrow m => BSL.ByteString -> m (Sigma ElfClass (TyCon1 HeadersXX))
parseHeaders bs = do
    ((classS :&: hxx) :: Header) <- elfDecodeOrFail bs
    withElfClass classS parseHeaders' hxx bs
