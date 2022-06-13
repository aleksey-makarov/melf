{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Prelude as P

import Control.Lens.Combinators
-- import Control.Lens.Fold
import Control.Monad
-- import Control.Monad.Catch
-- import Data.Binary
import Data.Bits
-- import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Foldable as F
-- import Data.Int
import Data.Singletons
-- import Data.Singletons.Sigma
import Data.Word
import Data.Function
import Numeric
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory
import System.FilePath
import System.IO as IO
import Test.Tasty
import Test.Tasty.Golden
-- import Test.Tasty.HUnit

-- import Control.Exception.ChainedException
-- import Data.Elf
-- import Data.Elf.PrettyPrint
import Data.Elf.Headers2
-- import Data.Endian

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldlM f ([], [])
    where
        f (ts, fs) x = do
            b <- p x
            return $ if b then (x:ts, fs) else (ts, x:fs)

traverseDir :: FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
traverseDir root ok = go root
    where
        go :: FilePath -> IO [FilePath]
        go dir = do
            paths <- P.map (dir </>) <$> listDirectory dir
            (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
            oks <- filterM ok filePaths
            (oks ++) <$> (F.concat <$> mapM go dirPaths)

readElfMagic :: FilePath -> IO ByteString
readElfMagic p = BSL.take 4 <$> BSL.readFile p

elfMagicBS :: ByteString
elfMagicBS = pack [ 0x7f, 0x45, 0x4c, 0x46 ] -- "\DELELF"

isElf :: FilePath -> IO Bool
isElf p = if takeExtension p == ".bad"
    then return False
    else (== elfMagicBS) <$> readElfMagic p

-- decodeOrFailAssertion :: Binary a => ByteString -> IO (Int64, a)
-- decodeOrFailAssertion bs = case decodeOrFail bs of
--     Left (_, off, err) -> assertFailure (err ++ " @" ++ show off)
--     Right (_, off, a) -> return (off, a)

-- mkTest'' :: forall a . IsElfClass a => HeaderXX a -> ByteString -> Assertion
-- mkTest'' HeaderXX{..} bs = do
-- 
--     let
--         takeLen off len = BSL.take (fromIntegral len) $ BSL.drop (fromIntegral off) bs
--         bsSections = takeLen hShOff (hShEntSize * hShNum)
--         bsSegments = takeLen hPhOff (hPhEntSize * hPhNum)
-- 
--     (ss :: [SectionXX a]) <- parseBList hData bsSections
--     assertEqual "Section table round trip does not work" bsSections $ serializeBList hData ss
-- 
--     (ps :: [SegmentXX a]) <- parseBList hData bsSegments
--     assertEqual "Segment table round trip does not work" bsSegments $ serializeBList hData ps

-- mkTest' :: ByteString -> Assertion
-- mkTest' bs = do
--     (off, elfh@(classS :&: hxx) :: Header) <- decodeOrFailAssertion bs
--     assertBool "Incorrect header size" (headerSize (fromSing classS) == off)
--     assertEqual "Header round trip does not work" (BSL.take off bs) (encode elfh)
-- 
--     withElfClass classS mkTest'' hxx bs

-- mkTest :: FilePath -> TestTree
-- mkTest p = testCase p $ withBinaryFile p ReadMode (BSL.hGetContents >=> mkTest')

mkGoldenTest' :: FilePath -> FilePath -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTest' g o formatFunction file = goldenVsFile file g o mkGoldenTestOutput
    where
        mkGoldenTestOutput :: IO ()
        mkGoldenTestOutput = do
            doc <- formatFunction file
            withFile o WriteMode (`hPutDoc` doc)

mkGoldenTest :: String -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTest name formatFunction file = mkGoldenTest' g o formatFunction file
    where
        newBase = "tests" </> file <.> name
        o = newBase <.> "out"
        g = newBase <.> "golden"

-- mkGoldenTestOSuffix :: String -> String -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
-- mkGoldenTestOSuffix name osuffix formatFunction file = mkGoldenTest' g o formatFunction file
--     where
--         newBase = "tests" </> file <.> name
--         o = newBase <.> osuffix <.> "out"
--         g = newBase <.> "golden"

------------------------------------------------------------------------------

-- FIXME: define foldMapRBuilderList

-- index' :: (Integral i, MonadThrow m) => [a] -> i -> m a
-- index' (x:_) 0 = return x
-- index' (_:xs) n | n > 0     = index' xs (n-1)
--                 | otherwise = $chainedError "index': negative argument."
-- index' _ _                  = $chainedError "index': index too large."

-- getStringTable :: MonadThrow m => Sigma ElfClass (TyCon1 HeadersXX) -> BSL.ByteString -> m BSL.ByteString
-- getStringTable (classS :&: HeadersXX (HeaderXX{..}, ss, _)) bs = withElfClass classS
--     if hShStrNdx == 0
--         then return BSL.empty
--         else do
--             strs <- index' ss hShStrNdx
--             return $ getSectionData bs strs

-- copyElf :: MonadCatch m => BSL.ByteString -> m BSL.ByteString
-- copyElf bs = parseElf bs >>= serializeElf

---------------------------------------------------------------------

-- | Splits an integer into list of integers such that its sum equals to the argument,
--   and each element of the list is of the form @(1 << x)@ for some @x@.
--   @splitBits 5@ produces @[ 1, 4 ]@
splitBits :: (Num w, FiniteBits w) => w -> [w]
splitBits w = fmap (shiftL 1) $ P.filter (testBit w) $ fmap (subtract 1) [ 1 .. (finiteBitSize w) ]

padLeadingZeros :: Int -> String -> String
padLeadingZeros n s | P.length s > n = error "padLeadingZeros args"
                    | otherwise      = "0x" ++ P.replicate (n - P.length s) '0' ++ s

-- printWord8 :: Word8 -> Doc ()
-- printWord8 n = pretty $ padLeadingZeros 2 $ showHex n ""

printWord16 :: Word16 -> Doc ()
printWord16 n = pretty $ padLeadingZeros 4 $ showHex n ""

printWord32 :: Word32 -> Doc ()
printWord32 n = pretty $ padLeadingZeros 8 $ showHex n ""

printWord64 :: Word64 -> Doc ()
printWord64 n = pretty $ padLeadingZeros 16 $ showHex n ""

printWordXXS :: Sing a -> WordXX a -> Doc ()
printWordXXS SELFCLASS32 = printWord32
printWordXXS SELFCLASS64 = printWord64

printWordXX :: SingI a => WordXX a -> Doc ()
printWordXX = withSing printWordXXS

formatPairs :: [(String, Doc a)] -> Doc a
formatPairs ls = align $ vsep $ fmap f ls
    where
        f (n, v) = fill w (pretty n <> ":") <+> v
        w = 1 + P.maximum (fmap (P.length . fst) ls)

formatFold :: Fold s (Doc ()) -> s -> (Doc ())
formatFold fd s = align $ vsep $ toListOf (fd . to f) s
    where
        f x = pretty '-' <+> x

printSection :: IsElfClass a => SectionXX a -> Doc ()
printSection s =
    formatPairs
        [ ("N",         viaShow     $ (0 :: Int)  )
        , ("Name",      viaShow     $ sName      s) -- Word32
        , ("Type",      viaShow     $ sType      s) -- ElfSectionType
        , ("Flags",     printWordXX $ sFlags     s) -- WordXX c
        , ("Addr",      printWordXX $ sAddr      s) -- WordXX c
        , ("Offset",    printWordXX $ sOffset    s) -- WordXX c
        , ("Size",      printWordXX $ sSize      s) -- WordXX c
        , ("Link",      viaShow     $ sLink      s) -- Word32
        , ("Info",      viaShow     $ sInfo      s) -- Word32
        , ("AddrAlign", printWordXX $ sAddrAlign s) -- WordXX c
        , ("EntSize",   printWordXX $ sEntSize   s) -- WordXX c
        ]

printSegment :: IsElfClass a => SegmentXX a -> Doc ()
printSegment p =
    formatPairs
        [ ("N",        viaShow             $ (0 :: Int) )
        , ("Type",     viaShow             $ pType     p) -- ElfSegmentType
        , ("Flags",    viaShow $ splitBits $ pFlags    p) -- ElfSegmentFlag
        , ("Offset",   printWordXX         $ pOffset   p) -- WordXX c
        , ("VirtAddr", printWordXX         $ pVirtAddr p) -- WordXX c
        , ("PhysAddr", printWordXX         $ pPhysAddr p) -- WordXX c
        , ("FileSize", printWordXX         $ pFileSize p) -- WordXX c
        , ("MemSize",  printWordXX         $ pMemSize  p) -- WordXX c
        , ("Align",    printWordXX         $ pAlign    p) -- WordXX c
        ]

printSections :: IsElfClass c => ELFXX c -> Doc ()
printSections elf = elf & formatFold (sections . to printSection)

printSegments :: IsElfClass c => ELFXX c -> Doc ()
printSegments elf = elf & formatFold (segmets . to printSegment)

printHeader :: IsElfClass c => ELFXX c -> Doc ()
printHeader elf = formatPairs
    [ ( "Class",      viaShow     $ hClass      elf)
    , ( "Data",       viaShow     $ hData       elf)
    , ( "OSABI",      viaShow     $ hOSABI      elf)
    , ( "ABIVersion", viaShow     $ hABIVersion elf)
    , ( "Type",       viaShow     $ hType       elf)
    , ( "Machine",    viaShow     $ hMachine    elf)
    , ( "Entry",      printWordXX $ hEntry      elf)
    , ( "PhOff",      printWordXX $ hPhOff      elf)
    , ( "ShOff",      printWordXX $ hShOff      elf)
    , ( "Flags",      printWord32 $ hFlags      elf)
    , ( "PhEntSize",  printWord16 $ hPhEntSize  elf)
    , ( "PhNum",      viaShow     $ hPhNum      elf)
    , ( "ShEntSize",  printWord16 $ hShEntSize  elf)
    , ( "ShNum",      viaShow     $ hShNum      elf)
    , ( "ShStrNdx",   viaShow     $ hShStrNdx   elf)
    ]

-- | Print parsed header, section table and segment table.
--   It's used in golden tests
printHeaders :: IsElfClass a => ELFXX a -> Doc ()
printHeaders elf = formatPairs
    [ ("Header",   printHeader   elf)
    , ("Sections", printSections elf)
    , ("Segments", printSegments elf)
    ]

printHeadersFile :: FilePath -> IO (Doc ())
printHeadersFile path = do
    elf <- readELF path
    return $ withELF elf printHeaders

-- printHeadersFile :: FilePath -> IO (Doc ())
-- printHeadersFile path = do
--     bs <- fromStrict <$> BS.readFile path
--     (classS :&: HeadersXX (hdr, ss, ps)) <- parseHeaders bs
--     return $ withSingI classS $ printHeaders hdr ss ps

-- printStrTableFile :: FilePath -> IO (Doc ())
-- printStrTableFile path = do
--     bs <- readFileLazy path
--     hdrs <- parseHeaders bs
--     st <- getStringTable hdrs bs
--     printStringTable st
-- 
-- printCopyStrTableFile :: FilePath -> IO (Doc ())
-- printCopyStrTableFile path = do
--     bs <- readFileLazy path
--     bs' <- copyElf bs
--     hdrs <- parseHeaders bs'
--     st <- getStringTable hdrs bs'
--     printStringTable st
-- 
-- printRBuilderFile :: FilePath -> IO (Doc ())
-- printRBuilderFile path = do
--     bs <- readFileLazy path
--     hdrs <- parseHeaders bs
--     printLayout hdrs bs
-- 
-- printCopyRBuilderFile :: FilePath -> IO (Doc ())
-- printCopyRBuilderFile path = do
--     bs <- readFileLazy path
--     bs' <- copyElf bs
--     hdrs <- parseHeaders bs'
--     printLayout hdrs bs'
-- 
-- printElfFile :: FilePath -> IO (Doc ())
-- printElfFile path = do
--     bs <- readFileLazy path
--     e <- parseElf bs
--     printElf e
-- 
-- printCopyElfFile :: FilePath -> IO (Doc ())
-- printCopyElfFile path = do
--     bs <- readFileLazy path
--     bs' <- copyElf bs
--     e <- parseElf bs'
--     printElf e

-----------------------------------------------------------------------

-- testHeader64 :: Header
-- testHeader64 = SELFCLASS64 :&: HeaderXX ELFDATA2LSB 0 0 0 0 0 0 0 0 0 0 0 0 0
-- 
-- testHeader32 :: Header
-- testHeader32 = SELFCLASS32 :&: HeaderXX ELFDATA2MSB 0 0 0 0 0 0 0 0 0 0 0 0 0
-- 
-- testSection64 :: SectionXX 'ELFCLASS64
-- testSection64 = SectionXX 0 0 0 0 0 0 0 0 0 0
-- 
-- testSection32 :: SectionXX 'ELFCLASS32
-- testSection32 = SectionXX 0 0 0 0 0 0 0 0 0 0
-- 
-- testSegment64 :: SegmentXX 'ELFCLASS64
-- testSegment64 =  SegmentXX 0 0 0 0 0 0 0 0
-- 
-- testSegment32 :: SegmentXX 'ELFCLASS32
-- testSegment32 =  SegmentXX 0 0 0 0 0 0 0 0
-- 
-- testSymbolTableEntry64 :: SymbolXX 'ELFCLASS64
-- testSymbolTableEntry64 =  SymbolXX 0 0 0 0 0 0
-- 
-- testSymbolTableEntry32 :: SymbolXX 'ELFCLASS32
-- testSymbolTableEntry32 =  SymbolXX 0 0 0 0 0 0
-- 
-- mkSizeTest :: Binary a => String -> a -> Int64 -> TestTree
-- mkSizeTest name v s = testCase name (len @?= s)
--     where
--         len = BSL.length $ encode v
-- 
-- hdrSizeTests :: TestTree
-- hdrSizeTests = testGroup "header size" [ mkSizeTest "header 64" testHeader64 (headerSize ELFCLASS64)
--                                        , mkSizeTest "header 32" testHeader32 (headerSize ELFCLASS32)
-- 
--                                        , mkSizeTest "section 64" (Le testSection64) (sectionTableEntrySize ELFCLASS64)
--                                        , mkSizeTest "section 32" (Be testSection32) (sectionTableEntrySize ELFCLASS32)
-- 
--                                        , mkSizeTest "segment 64" (Le testSegment64) (segmentTableEntrySize ELFCLASS64)
--                                        , mkSizeTest "segment 32" (Be testSegment32) (segmentTableEntrySize ELFCLASS32)
-- 
--                                        , mkSizeTest "symbol table entry 64" (Le testSymbolTableEntry64) (symbolTableEntrySize ELFCLASS64)
--                                        , mkSizeTest "symbol table entry 32" (Be testSymbolTableEntry32) (symbolTableEntrySize ELFCLASS32)
--                                        ]

main :: IO ()
main = do

    elfs <- traverseDir "testdata" isElf

    defaultMain $ testGroup "elf" [ testGroup "header golden" (mkGoldenTest "header" printHeadersFile <$> elfs)
                                  -- , testGroup "headers round trip"  (mkTest <$> elfs)
                                  --   hdrSizeTests
                                  -- , testGroup "string table golden" (mkGoldenTest        "strtable"        printStrTableFile     <$> elfs)
                                  -- , testGroup "layout golden"       (mkGoldenTest        "layout"          printRBuilderFile     <$> elfs)
                                  -- , testGroup "elf golden"          (mkGoldenTest        "elf"             printElfFile          <$> elfs)
                                  -- , testGroup "string table copy"   (mkGoldenTestOSuffix "strtable" "copy" printCopyStrTableFile <$> elfs)
                                  -- , testGroup "layout copy"         (mkGoldenTestOSuffix "layout"   "copy" printCopyRBuilderFile <$> elfs)
                                  -- , testGroup "elf copy"            (mkGoldenTestOSuffix "elf"      "copy" printCopyElfFile      <$> elfs)
                                  ]
