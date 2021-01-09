{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Data.Elf is a module for parsing a ByteString of an ELF file into an Elf record.
module Data.Elf.Headers
    ( ElfClass(..)
    , ElfData(..)

    , IsElfClass(..)
    , withElfClass

    , headerSize
    , sectionTableEntrySize
    , segmentTableEntrySize
    , symbolTableEntrySize
    , wordAlign

    , HeaderXX(..)
    , Header

    , BList(..)

    , SectionXX(..)
    , SegmentXX(..)
    , SymbolTableEntryXX(..)

    ---------------------------------

    , sectionIsSymbolTable

    , SElfClass (..)

    , splitBits

    , HeadersXX (..)
    , parseHeaders
    , parseListA
    , serializeListA

    , elfMagic

    , module Data.Elf.Generated) where

-- import Control.Lens hiding (at)
-- import Control.Arrow
import Control.Monad
import Control.Monad.Catch
-- import Control.Monad.State hiding (get, put)
-- import qualified Control.Monad.State as S
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString       as BS
import Data.ByteString.Lazy  as BSL
-- import Data.ByteString.Char8 as BSC
import Data.Data (Data)
import Data.Kind
-- import Data.Kind
import qualified Data.List as L
import Data.Singletons.Sigma
import Data.Singletons.TH
import Data.Typeable (Typeable)
-- import Numeric.Interval as I
-- import Numeric.Interval.NonEmpty as INE

-- https://stackoverflow.com/questions/10672981/export-template-haskell-generated-definitions

import Data.Elf.Exception
import Data.Elf.Generated

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

splitBits :: (Num w, FiniteBits w) => w -> [w]
splitBits w = fmap (shiftL 1) $ L.filter (testBit w) $ fmap (subtract 1) [ 1 .. (finiteBitSize w) ]

newtype BList a = BList { fromBList :: [a] }

instance Binary a => Binary (BList a) where
    put (BList (a:as)) = put a >> put (BList as)
    put (BList []) = return ()
    get = do
        e <- isEmpty
        if e then return $ BList [] else do
            a <- get
            (BList as) <- get
            return $ BList $ a : as

--------------------------------------------------------------------------
-- WordXX
--------------------------------------------------------------------------

type IsElfClass :: ElfClass -> Constraint
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
      ) => IsElfClass c where
    type WordXX c = r | r -> c

instance IsElfClass 'ELFCLASS32 where
    type WordXX 'ELFCLASS32 = Word32

instance IsElfClass 'ELFCLASS64 where
    type WordXX 'ELFCLASS64 = Word64

--------------------------------------------------------------------------
-- Header
--------------------------------------------------------------------------

type HeaderXX :: ElfClass -> Type
data HeaderXX c =
    HeaderXX
        { hData       :: ElfData
        , hOSABI      :: ElfOSABI
        , hABIVersion :: Word8
        , hType       :: ElfType
        , hMachine    :: ElfMachine
        , hEntry      :: WordXX c
        , hPhOff      :: WordXX c
        , hShOff      :: WordXX c
        , hFlags      :: Word32
        , hPhEntSize  :: Word16
        , hPhNum      :: Word16
        , hShEntSize  :: Word16
        , hShNum      :: Word16
        , hShStrNdx   :: Word16
        }

type Header = Sigma ElfClass (TyCon1 HeaderXX)

headerSize :: Num a => ElfClass -> a
headerSize ELFCLASS64 = 64
headerSize ELFCLASS32 = 52

sectionTableEntrySize :: Num a => ElfClass -> a
sectionTableEntrySize ELFCLASS64 = 64
sectionTableEntrySize ELFCLASS32 = 40

segmentTableEntrySize :: Num a => ElfClass -> a
segmentTableEntrySize ELFCLASS64 = 56
segmentTableEntrySize ELFCLASS32 = 32

symbolTableEntrySize :: Num a => ElfClass -> a
symbolTableEntrySize ELFCLASS64 = 24
symbolTableEntrySize ELFCLASS32 = 16

wordAlign :: Num a => ElfClass -> a
wordAlign ELFCLASS64 = 8
wordAlign ELFCLASS32 = 4

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
    when (hSize /= (headerSize $ fromSing classS)) $ error "incorrect size of elf header"
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

data SectionXX (c :: ElfClass) =
    SectionXX
        { sName      :: Word32
        , sType      :: ElfSectionType
        , sFlags     :: WordXX c
        , sAddr      :: WordXX c
        , sOffset    :: WordXX c
        , sSize      :: WordXX c
        , sLink      :: Word32
        , sInfo      :: Word32
        , sAddrAlign :: WordXX c
        , sEntSize   :: WordXX c
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
    put = (withElfClass (sing @ a) (putSection putBe)) . fromBe
    get = Be <$> (withElfClass (sing @ a) (getSection getBe))

instance forall (a :: ElfClass) . SingI a => Binary (Le (SectionXX a)) where
    put = (withElfClass (sing @ a) (putSection putLe)) . fromLe
    get = Le <$> (withElfClass (sing @ a) (getSection getLe))

--------------------------------------------------------------------------
-- Segment
--------------------------------------------------------------------------

data SegmentXX (c :: ElfClass) =
    SegmentXX
        { pType     :: ElfSegmentType
        , pFlags    :: Word32
        , pOffset   :: WordXX c
        , pVirtAddr :: WordXX c
        , pPhysAddr :: WordXX c
        , pFileSize :: WordXX c
        , pMemSize  :: WordXX c
        , pAlign    :: WordXX c
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
-- symbol table entry
--------------------------------------------------------------------------

sectionIsSymbolTable :: ElfSectionType -> Bool
sectionIsSymbolTable sType  = sType `L.elem` [SHT_SYMTAB, SHT_DYNSYM]

data SymbolTableEntryXX (c :: ElfClass) =
    SymbolTableEntryXX
        { stName  :: Word32
        , stInfo  :: Word8
        , stOther :: Word8
        , stShNdx :: ElfSectionIndex
        , stValue :: WordXX c
        , stSize  :: WordXX c
        }

getSymbolTableEntry ::    forall (c :: ElfClass) . Sing c ->
     (forall b . (Binary (Le b), Binary (Be b)) => Get b) -> Get (SymbolTableEntryXX c)
getSymbolTableEntry SELFCLASS64 getE = do

    stName  <- getE
    stInfo  <- get
    stOther <- get
    stShNdx <- getE
    stValue <- getE
    stSize  <- getE

    return SymbolTableEntryXX{..}

getSymbolTableEntry SELFCLASS32 getE = do

    stName  <- getE
    stValue <- getE
    stSize  <- getE
    stInfo  <- get
    stOther <- get
    stShNdx <- getE

    return SymbolTableEntryXX{..}

putSymbolTableEntry ::      forall (c :: ElfClass) . Sing c ->
    (forall b . (Binary (Le b), Binary (Be b)) => b -> Put) ->
                                       SymbolTableEntryXX c -> Put
putSymbolTableEntry SELFCLASS64 putE (SymbolTableEntryXX{..}) = do

    putE stName
    put  stInfo
    put  stOther
    putE stShNdx
    putE stValue
    putE stSize

putSymbolTableEntry SELFCLASS32 putE (SymbolTableEntryXX{..}) = do

    putE stName
    putE stValue
    putE stSize
    put  stInfo
    put  stOther
    putE stShNdx

instance forall (a :: ElfClass) . SingI a => Binary (Be (SymbolTableEntryXX a)) where
    put = putSymbolTableEntry sing putBe . fromBe
    get = Be <$> getSymbolTableEntry sing getBe

instance forall (a :: ElfClass) . SingI a => Binary (Le (SymbolTableEntryXX a)) where
    put = putSymbolTableEntry sing putLe . fromLe
    get = Le <$> getSymbolTableEntry sing getLe

--------------------------------------------------------------------------
-- parseHeaders
--------------------------------------------------------------------------

-- FIXME: how to get rid of this? (use some combinators for Sigma)
newtype HeadersXX a = HeadersXX (HeaderXX a, [SectionXX a], [SegmentXX a])
-- type ElfHeadersXX a = (HeaderXX a, SectionXX a, SegmentXX a)

elfDecodeOrFail' :: (Binary a, MonadThrow m) => BSL.ByteString -> m (ByteOffset, a)
elfDecodeOrFail' bs = case decodeOrFail bs of
    Left (_, off, err) -> $elfError $ err ++ " @" ++ show off
    Right (_, off, a) -> return (off, a)

elfDecodeOrFail :: (Binary a, MonadThrow m) => BSL.ByteString -> m a
elfDecodeOrFail bs = snd <$> elfDecodeOrFail' bs

elfDecodeAllOrFail :: (Binary a, MonadThrow m) => BSL.ByteString -> m a
elfDecodeAllOrFail bs = do
    (off, a) <- elfDecodeOrFail' bs
    if off == (BSL.length bs) then return a else $elfError $ "leftover != 0 @" ++ show off

-- FIXME: these should be instance Binary (Le a) => Binary (Le (Blist a))
parseListA :: (MonadThrow m, Binary (Le a), Binary (Be a)) => ElfData -> BSL.ByteString -> m [a]
parseListA d bs = case d of
    ELFDATA2LSB -> fmap fromLe <$> fromBList <$> elfDecodeAllOrFail bs
    ELFDATA2MSB -> fmap fromBe <$> fromBList <$> elfDecodeAllOrFail bs

serializeListA :: (Binary (Le a), Binary (Be a)) => ElfData -> [a] -> BSL.ByteString
serializeListA d as = case d of
    ELFDATA2LSB -> encode $ BList $ fmap Le $ as
    ELFDATA2MSB -> encode $ BList $ fmap Be $ as

parseHeaders' :: (IsElfClass a, MonadThrow m) => HeaderXX a -> BSL.ByteString -> m (Sigma ElfClass (TyCon1 HeadersXX))
parseHeaders' hxx@HeaderXX{..} bs =
    let
        takeLen off len = BSL.take (fromIntegral len) $ BSL.drop (fromIntegral off) bs
        bsSections = takeLen hShOff (hShEntSize * hShNum)
        bsSegments = takeLen hPhOff (hPhEntSize * hPhNum)
    in do
        ss <- parseListA hData bsSections
        ps <- parseListA hData bsSegments
        return $ sing :&: HeadersXX (hxx, ss, ps)

parseHeaders :: MonadThrow m => BSL.ByteString -> m (Sigma ElfClass (TyCon1 HeadersXX))
parseHeaders bs = do
    ((classS :&: hxx) :: Header) <- elfDecodeOrFail bs
    (withElfClass classS parseHeaders') hxx bs
