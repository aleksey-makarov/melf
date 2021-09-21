-- |
-- Module      : Data.Elf.PrettyPrint
-- Description : Pretty printing the data parsed by Data.Elf
-- Copyright   : (c) Aleksey Makarov, 2021
-- License     : BSD 3-Clause License
-- Maintainer  : aleksey.makarov@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Pretty print the data parsed by @Data.Elf@.  Basically these functions are used for golden testing.

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Elf.PrettyPrint
    ( printHeaders
    , printLayout
    , printElf_
    , printElf
    , printStringTable

    , readFileLazy
    , writeElfDump
    , writeElfLayout

    , splitBits
    ) where

import Control.Monad
import Control.Monad.Catch
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Int
import qualified Data.List as L
import Data.Monoid
import Data.Singletons
import Data.Singletons.Sigma
import Data.Text.Prettyprint.Doc as D
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Word
import Numeric
import System.IO

import Control.Exception.ChainedException
import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers
import Data.Interval

-- | Splits an integer into list of integers such that its sum equals to the argument,
--   and each element of the list is of the form @(1 << x)@ for some @x@.
--   @splitBits 5@ produces @[ 1, 4 ]@
splitBits :: (Num w, FiniteBits w) => w -> [w]
splitBits w = fmap (shiftL 1) $ L.filter (testBit w) $ fmap (subtract 1) [ 1 .. (finiteBitSize w) ]

formatPairs :: [(String, Doc a)] -> Doc a
formatPairs ls = align $ vsep $ fmap f ls
    where
        f (n, v) = fill w (pretty n <> ":") <+> v
        w = 1 + (maximum $ fmap (length . fst) ls)

formatList :: [Doc ()] -> Doc ()
formatList = align . vsep . fmap f
    where
        f x = pretty '-' <+> x

padLeadingZeros :: Int -> String -> String
padLeadingZeros n s | length s > n = error "padLeadingZeros args"
                    | otherwise = "0x" ++ replicate (n - length s) '0' ++ s

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

printHeader :: forall a . SingI a => HeaderXX a -> Doc ()
printHeader HeaderXX{..} =
    formatPairs
        [ ("Class",      viaShow $ fromSing $ sing @a )
        , ("Data",       viaShow hData           ) -- ElfData
        , ("OSABI",      viaShow hOSABI          ) -- ElfOSABI
        , ("ABIVersion", viaShow hABIVersion     ) -- Word8
        , ("Type",       viaShow hType           ) -- ElfType
        , ("Machine",    viaShow hMachine        ) -- ElfMachine
        , ("Entry",      printWordXX hEntry      ) -- WordXX c
        , ("PhOff",      printWordXX hPhOff      ) -- WordXX c
        , ("ShOff",      printWordXX hShOff      ) -- WordXX c
        , ("Flags",      printWord32 hFlags      ) -- Word32
        , ("PhEntSize",  printWord16 hPhEntSize  ) -- Word16
        , ("PhNum",      viaShow hPhNum          ) -- Word16
        , ("ShEntSize",  printWord16  hShEntSize ) -- Word16
        , ("ShNum",      viaShow hShNum          ) -- Word16
        , ("ShStrNdx",   viaShow hShStrNdx       ) -- Word16
        ]

printSection :: SingI a => (Int, SectionXX a) -> Doc ()
printSection (n, SectionXX{..}) =
    formatPairs $
        [ ("N",         viaShow n              )
        , ("Name",      viaShow sName          ) -- Word32
        , ("Type",      viaShow sType          ) -- ElfSectionType
        , ("Flags",     printWordXX sFlags     ) -- WordXX c
        , ("Addr",      printWordXX sAddr      ) -- WordXX c
        , ("Offset",    printWordXX sOffset    ) -- WordXX c
        , ("Size",      printWordXX sSize      ) -- WordXX c
        , ("Link",      viaShow sLink          ) -- Word32
        , ("Info",      viaShow sInfo          ) -- Word32
        , ("AddrAlign", printWordXX sAddrAlign ) -- WordXX c
        , ("EntSize",   printWordXX sEntSize   ) -- WordXX c
        ]

printSegment :: SingI a => (Int, SegmentXX a) -> Doc ()
printSegment (n, SegmentXX{..}) =
    formatPairs
        [ ("N",        viaShow n             )
        , ("Type",     viaShow pType         ) -- ElfSegmentType
        , ("Flags",    viaShow $ splitBits pFlags ) -- ElfSegmentFlag
        , ("Offset",   printWordXX pOffset   ) -- WordXX c
        , ("VirtAddr", printWordXX pVirtAddr ) -- WordXX c
        , ("PhysAddr", printWordXX pPhysAddr ) -- WordXX c
        , ("FileSize", printWordXX pFileSize ) -- WordXX c
        , ("MemSize",  printWordXX pMemSize  ) -- WordXX c
        , ("Align",    printWordXX pAlign    ) -- WordXX c
        ]

-- | Print parsed headers.  It's used in golden tests
printHeaders :: SingI a => HeaderXX a -> [SectionXX a] -> [SegmentXX a] -> Doc ()
printHeaders hdr ss ps =
    let
        h  = printHeader hdr
        s  = fmap printSection (Prelude.zip [0 .. ] ss)
        p  = fmap printSegment (Prelude.zip [0 .. ] ps)
    in
        formatPairs
            [ ("Header",       h)
            , ("Sections",     formatList s)
            , ("Segments",     formatList p)
            ]

--------------------------------------------------------------------
--
--------------------------------------------------------------------

printRBuilder :: IsElfClass a => (Word32 -> String) -> [RBuilder a] -> Doc ()
printRBuilder getStr rbs = vsep ldoc

    where

        mapL f (ix, sx, dx) = (ix, f sx, dx)
        getS (_, sx, _) = sx

        longest [] = 0
        longest rbs' = maximum $ fmap (length . getS) rbs'

        padL n s | length s > n = error "incorrect number of pad symbols for `padL`"
                 | otherwise = replicate (n - length s) ' ' ++ s

        equalize l = fmap (mapL (padL l))

        printLine (pos, g, doc) = hsep $ pretty g : printWord32 (fromIntegral pos) : doc
        ls = concat $ map printRBuilder' rbs
        len = longest ls
        ldoc = fmap printLine $ equalize len ls

        printRBuilder' rb = f rb
            where

                i@(I o s) = rBuilderInterval rb

                f RBuilderHeader{} =
                    [ (o,         "┎", ["H"])
                    , (o + s - 1, "┖", [])
                    ]
                f RBuilderSectionTable{ rbstHeader = HeaderXX{..} } =
                    if hShNum == 0
                        then []
                        else
                            [ (o,         "┎", ["ST", parens $ viaShow hShNum])
                            , (o + s - 1, "┖", [])
                            ]
                f RBuilderSegmentTable{ rbptHeader = HeaderXX{..} } =
                    if hPhNum == 0
                        then []
                        else
                            [ (o,         "┎", ["PT", parens $ viaShow hPhNum])
                            , (o + s - 1, "┖", [])
                            ]
                f RBuilderSection{ rbsHeader = SectionXX{..}, ..} =
                    let
                        doc = [ "S" <> viaShow rbsN
                              , dquotes $ pretty $ getStr sName
                              , viaShow sType
                              , viaShow $ splitBits $ ElfSectionFlag $ fromIntegral sFlags
                              ]
                    in
                        if empty i
                            then
                                [(o, "-", doc)]
                            else
                                [(o,         "╓", doc)
                                ,(o + s - 1, "╙", [])
                                ]
                f RBuilderSegment{ rbpHeader = SegmentXX{..}, ..} =
                    let
                        doc = [ "P"
                              , viaShow pType
                              , viaShow $ splitBits pFlags
                              ]
                    in
                        if empty i && L.null rbpData
                            then
                                [(o, "-", doc)]
                            else
                                let
                                    xs = concat $ fmap printRBuilder' rbpData
                                    l = longest xs
                                    appendSectionBar = fmap (mapL ('│' : ))
                                    xsf = appendSectionBar $ equalize l xs
                                    b = '┌' : ((replicate l '─'))
                                    e = '└' : ((replicate l '─'))
                                in
                                    [(o,                                b, doc)] ++
                                    xsf                                          ++
                                    [(if empty i then o else o + s - 1, e, [] )]
                f RBuilderRawData{} =
                    let
                        doc = [ "R" ]
                    in
                        [(o,         "╓", doc)
                        ,(o + s - 1, "╙", [])
                        ]
                f RBuilderRawAlign{} = []

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

-- | Print ELF layout.  First parse ELF with `parseHeaders`, then use this function to
--   format the layout.
printLayout :: MonadCatch m => (Sigma ElfClass (TyCon1 HeadersXX)) -> BSL.ByteString -> m (Doc ())
printLayout (classS :&: HeadersXX (hdr, ss, ps)) bs = withElfClass classS do
    rbs <- parseRBuilder hdr ss ps bs
    let
        stringSectionData = getSectionData bs <$> findStringSection rbs
        getString' n = case stringSectionData of
            Nothing -> error "no string table"
            Just st -> getString st $ fromIntegral n
    return $ printRBuilder getString' rbs

--------------------------------------------------------------------
--
--------------------------------------------------------------------

formatPairsBlock :: Doc a -> [(String, Doc a)] -> Doc a
formatPairsBlock name pairs = vsep [ name <+> "{", indent 4 $ formatPairs pairs, "}" ]

printElfSymbolTableEntry :: SingI a => ElfSymbolXX a -> Doc ()
printElfSymbolTableEntry ElfSymbolXX{..} =
    formatPairsBlock ("symbol" <+> (dquotes $ pretty steName))
        [ ("Bind",  viaShow steBind      ) -- ElfSymbolBinding
        , ("Type",  viaShow steType      ) -- ElfSymbolType
        , ("ShNdx", viaShow steShNdx     ) -- ElfSectionIndex
        , ("Value", printWordXX steValue ) -- WordXX c
        , ("Size",  printWordXX steSize  ) -- WordXX c
        ]

printElfSymbolTable :: SingI a => Bool -> [ElfSymbolXX a] -> Doc ()
printElfSymbolTable full l = if full then printElfSymbolTableFull else printElfSymbolTable'
    where
        printElfSymbolTableFull = align . vsep $ fmap printElfSymbolTableEntry l
        printElfSymbolTable' = align . vsep $
            case l of
                (e1 : e2 : _ : _ : _) ->
                    [ printElfSymbolTableEntry e1
                    , printElfSymbolTableEntry e2
                    , "..."
                    , printElfSymbolTableEntry $ last l
                    , "total:" <+> viaShow (L.length l)
                    ]
                _ -> fmap printElfSymbolTableEntry l

splitBy :: Int64 -> BSL.ByteString -> [BSL.ByteString]
splitBy n = L.unfoldr f
    where
        f s | BSL.null s = Nothing
            | otherwise  = Just $ BSL.splitAt n s

formatChar :: Char -> Doc ()
formatChar c = pretty $ if isAscii c && not (isControl c) then c else '.'

formatHex :: Word8 -> Doc ()
formatHex w = pretty $ case showHex w "" of
    [ d ] -> [ '0', d ]
    ww -> ww

formatBytestringChar :: BSL.ByteString -> Doc ()
formatBytestringChar = hcat . L.map formatChar . BSL8.unpack

formatBytestringHex :: BSL.ByteString -> Doc ()
formatBytestringHex = hsep . L.map formatHex . BSL.unpack

formatBytestringLine :: BSL.ByteString -> Doc ()
formatBytestringLine s = (fill (16 * 2 + 15) $ formatBytestringHex s)
                      <+> pretty '#'
                      <+> formatBytestringChar s

printData :: Bool -> BSL.ByteString -> Doc ()
printData full bs = if full then printDataFull else printData'
    where
        printDataFull = align $ vsep $ L.map formatBytestringLine $ splitBy 16 bs
        printData' = align $ vsep $
            case splitBy 16 bs of
                (c1 : c2 : _ : _ : _) ->
                    [ formatBytestringLine c1
                    , formatBytestringLine c2
                    , "..."
                    , formatBytestringLine cl
                    , "total:" <+> viaShow (BSL.length bs)
                    ]
                chunks -> L.map formatBytestringLine chunks
        cl = BSL.drop (BSL.length bs - 16) bs

printElfSymbolTableEntryLine :: SingI a => ElfSymbolXX a -> Doc ()
printElfSymbolTableEntryLine ElfSymbolXX{..} =  parens ((dquotes $ pretty steName)
                                                    <+> "bind:"   <+> viaShow steBind
                                                    <+> "type:"   <+> viaShow steType
                                                    <+> "sindex:" <+> viaShow steShNdx
                                                    <+> "value:"  <+> printWordXX steValue
                                                    <+> "size:"   <+> printWordXX steSize)

printRelocationTableA_AARCH64 :: MonadThrow m => Bool -> Word32 -> [ElfXX 'ELFCLASS64] -> BSL.ByteString -> m (Doc ())
printRelocationTableA_AARCH64 full sLink elfs bs = do
    symTableSection <- elfFindSection elfs sLink
    symTable <- parseSymbolTable ELFDATA2LSB symTableSection elfs
    let
        getSymbolTableEntry' []     _  = $chainedError "wrong symbol table index"
        getSymbolTableEntry' (x:_)  0  = return x
        getSymbolTableEntry' (_:xs) n  = getSymbolTableEntry' xs (n - 1)

        getSymbolTableEntry :: MonadThrow m => Word32 -> m (ElfSymbolXX 'ELFCLASS64)
        getSymbolTableEntry = getSymbolTableEntry' symTable

        f :: MonadThrow m => RelaXX 'ELFCLASS64 -> m (Doc ())
        f RelaXX{..} = do
            symbolTableEntry <- getSymbolTableEntry relaSym
            return $  printWord64 relaOffset
                  <+> printWord64 relaAddend
                  <+> viaShow (ElfRelocationType_AARCH64 relaType)
                  <+> printElfSymbolTableEntryLine symbolTableEntry

        split xs = if full then xs else
            case xs of
                (x1 : x2 : _ : _ : _) ->
                    [ x1, x2, "...", last xs, "total:" <+> viaShow (length xs) ]
                _ -> xs

    relas <- parseBList ELFDATA2LSB bs
    (align . vsep . split) <$> mapM f relas

-- | Same as @`printElf_` False@
printElf :: MonadThrow m => Elf -> m (Doc ())
printElf = printElf_ False

-- | Print ELF.  If first argument is False, don't dump all the data, print just the first two and the last lines.
printElf_ :: MonadThrow m => Bool -> Elf -> m (Doc ())
printElf_ full (classS :&: ElfList elfs) = withElfClass classS do

    (hData, hMachine) <- do
        header <- elfFindHeader elfs
        case header of
            ElfHeader{..} -> return (ehData, ehMachine)
            _ -> $chainedError "not a header" -- FIXME

    let

        printElf' elfs' = align . vsep <$> mapM printElf'' elfs'

        printElf'' ElfHeader{..} =
            return $ formatPairsBlock "header"
                [ ("Class",      viaShow $ fromSing classS )
                , ("Data",       viaShow ehData       ) -- ElfData
                , ("OSABI",      viaShow ehOSABI      ) -- ElfOSABI
                , ("ABIVersion", viaShow ehABIVersion ) -- Word8
                , ("Type",       viaShow ehType       ) -- ElfType
                , ("Machine",    viaShow ehMachine    ) -- ElfMachine
                , ("Entry",      printWordXX ehEntry  ) -- WordXX c
                , ("Flags",      printWord32 ehFlags  ) -- Word32
                ]
        printElf'' s@ElfSection{ esData = (ElfSectionData bs), ..} = do
            (sectionName, dataDoc) <- if sectionIsSymbolTable esType
                then do
                    stes <- parseSymbolTable hData s elfs
                    return ("symbol table section", if null stes then "" else line <> (indent 4 $ printElfSymbolTable full stes))
                else if hMachine == EM_AARCH64
                        && hData == ELFDATA2LSB
                       && esType == SHT_RELA
                    && esEntSize == withElfClass classS relocationTableAEntrySize then
                        case classS of
                            SELFCLASS64 -> ("section", ) <$> printRelocationTableA_AARCH64 full esLink elfs bs
                            SELFCLASS32 -> $chainedError "invalid ELF: EM_AARCH64 and ELFCLASS32"
                else
                    return ("section", printData full bs)
            return $ formatPairsBlock (sectionName <+> (viaShow esN) <+> (dquotes $ pretty esName))
                [ ("Type",       viaShow esType          )
                , ("Flags",      viaShow $ splitBits esFlags )
                , ("Addr",       printWordXX esAddr      )
                , ("AddrAlign",  printWordXX esAddrAlign )
                , ("EntSize",    printWordXX esEntSize   )
                , ("Info",       printWord32 esInfo      )
                , ("Link",       printWord32 esLink      )
                , ("Data",       dataDoc )
                ]
        printElf'' ElfSection{ esData = ElfSectionDataStringTable, ..} =
            return $ "string table section" <+> (viaShow esN) <+> (dquotes $ pretty esName)
        printElf'' ElfSegment{..} = do
            dataDoc <- if null epData
                then return ""
                else do
                    dataDoc' <- printElf' epData
                    return $ line <> (indent 4 dataDoc')
            return $ formatPairsBlock "segment"
                [ ("Type",       viaShow epType         )
                , ("Flags",      viaShow $ splitBits epFlags )
                , ("VirtAddr",   printWordXX epVirtAddr )
                , ("PhysAddr",   printWordXX epPhysAddr )
                , ("MemSize",    printWordXX epMemSize  )
                , ("Align",      printWordXX epAlign    )
                , ("Data",       dataDoc                )
                ]
        printElf'' ElfSectionTable = return "section table"
        printElf'' ElfSegmentTable = return "segment table"
        printElf'' ElfRawData{..} =
            return $ formatPairsBlock "raw data"
                [ ("Data",       printData full edData)
                ]
        printElf'' ElfRawAlign{..} =
            return $ formatPairsBlock "raw align"
                [ ("Offset", printWordXX eaOffset )
                , ("Align",  printWordXX eaAlign  )
                ]

    printElf' elfs

--------------------------------------------------------------------
--
--------------------------------------------------------------------

-- | Print string table.  It's used in golden tests
printStringTable :: MonadThrow m => BSL.ByteString -> m (Doc ())
printStringTable bs =
    case BSL.unsnoc bs of
        Nothing -> return ""
        Just (bs', e) -> do
            when (e /= 0) $ $chainedError "string table should end with 0"
            return if (BSL.length bs' == 0)
                then angles ""
                else vsep $ map (angles . pretty) $ L.sort $ map BSL8.unpack $ BSL.splitWith (== 0) $ bs'

--------------------------------------------------------------------
--
--------------------------------------------------------------------

-- | Read the file strictly but return lazy bytestring
readFileLazy :: FilePath -> IO BSL.ByteString
readFileLazy path = BSL.fromStrict <$> BS.readFile path

-- | Read ELF from one file, `printElf` it into another.
writeElfDump :: FilePath -> FilePath -> IO ()
writeElfDump i o = do
    bs <- readFileLazy i
    e <- parseElf bs
    doc <- printElf e
    withFile o WriteMode (\ h -> hPutDoc h (doc <> line))

-- | Read ELF from one file, `printLayout` it into another.
writeElfLayout :: FilePath -> FilePath -> IO ()
writeElfLayout i o = do
    bs <- readFileLazy i
    hdrs <- parseHeaders bs
    doc <- printLayout hdrs bs
    withFile o WriteMode (\ h -> hPutDoc h (doc <> line))
