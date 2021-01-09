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

-- import Paths_elf
import Prelude as P

import Control.Arrow
import Control.Monad
import Control.Monad.Catch
import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BSL
-- import Data.ByteString.Lazy.Char8 as BSC
import Data.Foldable as F
-- import Data.Functor.Identity
import Data.Int
-- import Data.List as L
import Data.Monoid
import Data.Singletons
import Data.Singletons.Sigma
import Data.Text.Prettyprint.Doc as D
import Data.Text.Prettyprint.Doc.Render.Text
import System.Directory
import System.FilePath
import System.IO as IO
-- import System.Process.Typed
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Data.Elf
import Data.Elf.PrettyPrint
import Data.Elf.Exception
import Data.Elf.Headers

-- runExecWithStdoutFile :: FilePath -> [String] -> FilePath -> IO ()
-- runExecWithStdoutFile execFilePath args stdoutPath =
--     withBinaryFile stdoutPath WriteMode (\ oh -> do
--         let cfg = setStdout (useHandleClose oh) $ proc execFilePath args
--         runProcess_ cfg
--     )

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p l = foldlM f ([], []) l
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
            (oks ++) <$> (F.concat <$> (sequence $ P.map go dirPaths))

isElf :: FilePath -> IO Bool
isElf p = if takeExtension p == ".bad" then return False else (elfMagic ==) . decode <$> BSL.readFile p

decodeOrFailAssertion :: Binary a => ByteString -> IO (Int64, a)
decodeOrFailAssertion bs = case decodeOrFail bs of
    Left (_, off, err) -> assertFailure (err ++ " @" ++ show off)
    Right (_, off, a) -> return (off, a)

mkTest'' :: forall a . IsElfClass a => HeaderXX a -> ByteString -> Assertion
mkTest'' HeaderXX{..} bs = do

    let
        takeLen off len = BSL.take (fromIntegral len) $ BSL.drop (fromIntegral off) bs
        bsSections = takeLen hShOff (hShEntSize * hShNum)
        bsSegments = takeLen hPhOff (hPhEntSize * hPhNum)

    (off, s :: [SectionXX a]) <- withSingI (sing @ a) $ case hData of -- FIXME: use parse/serializeListA (?)
        ELFDATA2LSB -> second (fmap fromLe . fromBList) <$> (decodeOrFailAssertion bsSections)
        ELFDATA2MSB -> second (fmap fromBe . fromBList) <$> (decodeOrFailAssertion bsSections)

    assertEqual "Not all section table could be parsed" (BSL.length bsSections) off
    let
        encoded = case hData of -- FIXME: use parse/serializeListA (?)
            ELFDATA2LSB -> encode $ BList $ Le <$> s
            ELFDATA2MSB -> encode $ BList $ Be <$> s
    assertEqual "Section table round trip does not work" bsSections encoded

    (offp, p :: [SegmentXX a]) <- case hData of  -- FIXME: use parse/serializeListA (?)
        ELFDATA2LSB -> second (fmap fromLe . fromBList) <$> (decodeOrFailAssertion bsSegments)
        ELFDATA2MSB -> second (fmap fromBe . fromBList) <$> (decodeOrFailAssertion bsSegments)

    assertEqual "Not all ssgment table could be parsed" (BSL.length bsSegments) offp
    let
        encodedp = case hData of -- FIXME: use parse/serializeListA (?)
            ELFDATA2LSB -> encode $ BList $ Le <$> p
            ELFDATA2MSB -> encode $ BList $ Be <$> p
    assertEqual "Segment table round trip does not work" bsSegments encodedp

    return ()

mkTest' :: ByteString -> Assertion
mkTest' bs = do
    (off, elfh@(classS :&: hxx) :: Header) <- decodeOrFailAssertion bs
    assertBool "Incorrect header size" ((headerSize ELFCLASS32 == off) || (headerSize ELFCLASS64 == off))
    assertEqual "Header round trip does not work" (BSL.take off bs) (encode elfh)

    (withElfClass classS mkTest'') hxx bs

mkTest :: FilePath -> TestTree
mkTest p = testCase p $ withBinaryFile p ReadMode (BSL.hGetContents >=> mkTest')

mkGoldenTest' :: FilePath -> FilePath -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTest' g o formatFunction file = goldenVsFile file g o mkGoldenTestOutput
    where
        mkGoldenTestOutput :: IO ()
        mkGoldenTestOutput = do
            doc <- formatFunction file
            withFile o WriteMode (\ h -> hPutDoc h doc)

mkGoldenTest :: String -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTest name formatFunction file = mkGoldenTest' g o formatFunction file
    where
        newBase = "tests" </> file <.> name
        o = newBase <.> "out"
        g = newBase <.> "golden"

mkGoldenTestOSuffix :: String -> String -> (FilePath -> IO (Doc ())) -> FilePath -> TestTree
mkGoldenTestOSuffix name osuffix formatFunction file = mkGoldenTest' g o formatFunction file
    where
        newBase = "tests" </> file <.> name
        o = newBase <.> osuffix <.> "out"
        g = newBase <.> "golden"

------------------------------------------------------------------------------

findHeader :: SingI a => [RBuilder a] -> Maybe (HeaderXX a)
findHeader rbs = getFirst $ foldMap f rbs
    where
        f RBuilderSegment{..} = First $ findHeader rbpData
        f RBuilderHeader{ rbhHeader = h@HeaderXX{} } = First $ Just h
        f _ = First Nothing

findSection :: SingI a => Word16 -> [RBuilder a] -> Maybe (SectionXX a)
findSection n = findSection'
    where
        findSection' rbs = getFirst $ foldMap f rbs
        f RBuilderSegment{..} = First $ findSection' rbpData
        f RBuilderSection{..} = if n == rbsN then First $ Just rbsHeader else First Nothing
        f _ = First Nothing

findStringSection :: SingI a => [RBuilder a] -> Maybe (SectionXX a)
findStringSection rbs = do
    HeaderXX{..} <- findHeader rbs
    findSection hShStrNdx rbs

printRBuilder' :: MonadCatch m => (Sigma ElfClass (TyCon1 HeadersXX)) -> BSL.ByteString -> m (Doc ())
printRBuilder' (classS :&: HeadersXX (hdr, ss, ps)) bs = withElfClass classS do
    rbs <- parseRBuilder hdr ss ps bs
    let
        stringSectionData = getSectionData bs <$> findStringSection rbs
        getString' n = case stringSectionData of
            Nothing -> error "no string table"
            Just st -> getString st $ fromIntegral n
    return $ printRBuilder getString' rbs

readFileLazy :: FilePath -> IO BSL.ByteString
readFileLazy path = fromStrict <$> BS.readFile path

index' :: (Integral i, MonadThrow m) => [a] -> i -> m a
index' (x:_) 0 = return x
index' (_:xs) n | n > 0     = index' xs (n-1)
                | otherwise = $elfError "index': negative argument."
index' _ _                  = $elfError "index': index too large."

getStringTable :: MonadThrow m => (Sigma ElfClass (TyCon1 HeadersXX)) -> BSL.ByteString -> m BSL.ByteString
getStringTable (classS :&: HeadersXX (HeaderXX{..}, ss, _)) bs = withElfClass classS
    if hShStrNdx == 0
        then return BSL.empty
        else do
            strs <- index' ss hShStrNdx
            return $ getSectionData bs strs

copyElf :: MonadCatch m => BSL.ByteString -> m BSL.ByteString
copyElf bs = parseElf bs >>= serializeElf

---------------------------------------------------------------------

printHeadersFile :: FilePath -> IO (Doc ())
printHeadersFile path = do
    bs <- fromStrict <$> BS.readFile path
    (classS :&: HeadersXX (hdr, ss, ps)) <- parseHeaders bs
    return $ withSingI classS $ printHeaders hdr ss ps

printStrTableFile :: FilePath -> IO (Doc ())
printStrTableFile path = do
    bs <- readFileLazy path
    hdrs <- parseHeaders bs
    getStringTable hdrs bs >>= printStringTable

printCopyStrTableFile :: FilePath -> IO (Doc ())
printCopyStrTableFile path = do
    bs <- readFileLazy path >>= copyElf
    hdrs <- parseHeaders bs
    getStringTable hdrs bs >>= printStringTable

printRBuilderFile :: FilePath -> IO (Doc ())
printRBuilderFile path = do
    bs <- readFileLazy path
    hdrs <- parseHeaders bs
    printRBuilder' hdrs bs

printCopyRBuilderFile :: FilePath -> IO (Doc ())
printCopyRBuilderFile path = do
    bs <- readFileLazy path
    bs' <- copyElf bs
    hdrs <- parseHeaders bs'
    printRBuilder' hdrs bs'

printElfFile :: FilePath -> IO (Doc ())
printElfFile path = do
    bs <- readFileLazy path
    e <- parseElf bs
    printElf e

printCopyElfFile :: FilePath -> IO (Doc ())
printCopyElfFile path = do
    bs <- readFileLazy path
    bs' <- copyElf bs
    e <- parseElf bs'
    printElf e

-----------------------------------------------------------------------

testHeader64 :: Header
testHeader64 = SELFCLASS64 :&: (HeaderXX ELFDATA2LSB 0 0 0 0 0 0 0 0 0 0 0 0 0)

testHeader32 :: Header
testHeader32 = SELFCLASS32 :&: (HeaderXX ELFDATA2MSB 0 0 0 0 0 0 0 0 0 0 0 0 0)

testSection64 :: SectionXX 'ELFCLASS64
testSection64 = SectionXX 0 0 0 0 0 0 0 0 0 0

testSection32 :: SectionXX 'ELFCLASS32
testSection32 = SectionXX 0 0 0 0 0 0 0 0 0 0

testSegment64 :: SegmentXX 'ELFCLASS64
testSegment64 =  SegmentXX 0 0 0 0 0 0 0 0

testSegment32 :: SegmentXX 'ELFCLASS32
testSegment32 =  SegmentXX 0 0 0 0 0 0 0 0

testSymbolTableEntry64 :: SymbolTableEntryXX 'ELFCLASS64
testSymbolTableEntry64 =  SymbolTableEntryXX 0 0 0 0 0 0

testSymbolTableEntry32 :: SymbolTableEntryXX 'ELFCLASS32
testSymbolTableEntry32 =  SymbolTableEntryXX 0 0 0 0 0 0

mkSizeTest :: Binary a => String -> a -> Int64 -> TestTree
mkSizeTest name v s = testCase name (len @?= s)
    where
        len = BSL.length $ encode v

hdrSizeTests :: TestTree
hdrSizeTests = testGroup "header size" [ mkSizeTest "header 64" testHeader64 (headerSize ELFCLASS64)
                                       , mkSizeTest "header 32" testHeader32 (headerSize ELFCLASS32)

                                       , mkSizeTest "section 64" (Le testSection64) (sectionTableEntrySize ELFCLASS64)
                                       , mkSizeTest "section 32" (Be testSection32) (sectionTableEntrySize ELFCLASS32)

                                       , mkSizeTest "segment 64" (Le testSegment64) (segmentTableEntrySize ELFCLASS64)
                                       , mkSizeTest "segment 32" (Be testSegment32) (segmentTableEntrySize ELFCLASS32)

                                       , mkSizeTest "symbol table entry 64" (Le testSymbolTableEntry64) (symbolTableEntrySize ELFCLASS64)
                                       , mkSizeTest "symbol table entry 32" (Be testSymbolTableEntry32) (symbolTableEntrySize ELFCLASS32)
                                       ]

main :: IO ()
main = do

    elfs <- traverseDir "testdata" isElf

    defaultMain $ testGroup "elf" [ hdrSizeTests
                                  , testGroup "headers round trip"  (mkTest <$> elfs)
                                  , testGroup "headers golden"      (mkGoldenTest        "header"          printHeadersFile      <$> elfs)
                                  , testGroup "string table golden" (mkGoldenTest        "strtable"        printStrTableFile     <$> elfs)
                                  , testGroup "layout golden"       (mkGoldenTest        "layout"          printRBuilderFile     <$> elfs)
                                  , testGroup "elf golden"          (mkGoldenTest        "elf"             printElfFile          <$> elfs)
                                  , testGroup "string table copy"   (mkGoldenTestOSuffix "strtable" "copy" printCopyStrTableFile <$> elfs)
                                  , testGroup "layout copy"         (mkGoldenTestOSuffix "layout"   "copy" printCopyRBuilderFile <$> elfs)
                                  , testGroup "elf copy"            (mkGoldenTestOSuffix "elf"      "copy" printCopyElfFile      <$> elfs)
                                  ]
