{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Binary
import Data.ByteString as B
import Data.ByteString.Char8 as BC8
import Data.Char
import Data.Elf
import Data.Text.Prettyprint.Doc as D
import Data.Text.Prettyprint.Doc.Util
import Data.List as L
import Numeric
import System.Environment

showHexDoc :: (Integral a, Show a) => a -> Doc ()
showHexDoc n = pretty $ "0x" <> showHex n ""

formatBytestringChar :: ByteString -> Doc ()
formatBytestringChar = hcat . L.map formatChar . BC8.unpack

formatBytestringHex :: ByteString -> Doc ()
formatBytestringHex = hsep . L.map formatHex . B.unpack

formatBytestringLine :: ByteString -> Doc ()
formatBytestringLine s = (fill (16 * 2 + 15) $ formatBytestringHex s)
                      <+> pretty '#'
                      <+> formatBytestringChar s

splitBy :: Int -> ByteString -> [ByteString]
splitBy n = L.unfoldr f
    where
        f s | B.null s = Nothing
        f s | otherwise = Just $ B.splitAt n s

formatChar :: Char -> Doc ()
formatChar c = pretty $ if isAscii c && not (isControl c) then c else '.'

formatHex :: Word8 -> Doc ()
formatHex w = pretty $ case showHex w "" of
    [ d ] -> [ '0', d ]
    ww -> ww

formatBytestring :: ByteString -> Doc ()
formatBytestring = align . vsep . L.map formatBytestringLine . splitBy 16

formatPairs :: [(Doc a, Doc a)] -> Doc a
formatPairs ls = align $ vsep $ fmap f ls
    where
        f (n, v) = fill 10 (n <> ":") <+> v

formatList :: [Doc ()] -> Doc ()
formatList = align . vsep . fmap f
    where
        f x = pretty '-' <+> x

formatSymbol :: ElfSymbolTableEntry -> Doc ()
formatSymbol ste =
    formatPairs [ ("Name",         dquotes $ pretty $ nameToString $ steName ste)
                , ("Type",         viaShow $ steType ste)
                , ("Binding",      viaShow $ steBind ste)
                , ("Other",        showHexDoc $ steOther ste)
                , ("SectionIndex", viaShow $ steIndex ste)
                , ("Value",        showHexDoc $ steValue ste)
                , ("Size" ,        showHexDoc $ steSize ste)
                ]

formatSection :: ElfSection -> Doc ()
formatSection s =
    formatPairs ( ("Name",      dquotes $ pretty $ nameToString $ elfSectionName s)
                : ("Type",      viaShow $ elfSectionType s)
                : ("Flags",     formatList $ fmap viaShow $ splitBits $ elfSectionFlags s)
                : ("Addr",      showHexDoc $ elfSectionAddr s)
                : ("Size",      viaShow $ elfSectionSize s)
                : ("Link",      viaShow $ elfSectionLink s)
                : ("Info",      viaShow $ elfSectionInfo s)
                : ("AddrAlign", viaShow $ elfSectionAddrAlign s)
                : ("EntSize",   viaShow $ elfSectionEntSize s)
                : ("Data",      formatBytestring $ elfSectionData s)
                : case symbols of { [] -> []; _ -> [("Symbols", formatList symbolsDoc)] }
                )
    where
        symbols = elfParseSymbolTable s
        symbolsDoc = fmap formatSymbol symbols

formatSections :: [ElfSection] -> Doc ()
formatSections s = formatList  $ formatSection <$> s

formatSegment :: ElfSegment -> Doc ()
formatSegment s =
    formatPairs [ ("Type",     viaShow $ elfSegmentType s)
                , ("Flags",    formatList $ fmap viaShow $ splitBits $ elfSegmentFlags s)
                , ("VirtAddr", showHexDoc $ elfSegmentVirtAddr s)
                , ("PhysAddr", showHexDoc $ elfSegmentPhysAddr s)
                , ("Align",    viaShow $ elfSegmentAlign s)
                , ("Data",     formatBytestring $ elfSegmentData s)
                , ("MemSize",  viaShow $ elfSegmentMemSize s)
                ]

formatSegments :: [ElfSegment] -> Doc ()
formatSegments s = formatList $ formatSegment <$> s

formatElf :: Elf -> Doc ()
formatElf elf =
    formatPairs [ ("Class",      viaShow $ elfClass elf)
                , ("Data",       viaShow $ elfData elf)
                , ("Version",    viaShow $ elfVersion elf)
                , ("OSABI",      viaShow $ elfOSABI elf)
                , ("ABIVersion", viaShow $ elfABIVersion elf)
                , ("Type",       viaShow $ elfType elf)
                , ("Machine",    viaShow $ elfMachine elf)
                , ("Entry",      showHexDoc $ elfEntry elf)
                , ("Sections",   formatSections $ elfSections elf)
                , ("Segments",   formatSegments $ elfSegments elf)
                ]

printElf :: String -> IO ()
printElf fileName = do
    elf <- decodeFile fileName
    putDocW 80 $ formatElf elf <> line

main :: IO ()
main = do
    args <- getArgs
    mapM_ printElf args
