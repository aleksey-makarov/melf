{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Elf.PrettyPrint
    ( formatPairs
    , formatList
    , formatPairsBlock
    , printHeader
    , printSection
    , printSegment
    , printHeaders
    , printRBuilder
    , printElf
    , printStringTable
    ) where

import Control.Monad
import Control.Monad.Catch
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Int
import qualified Data.List as L
import Data.Singletons
import Data.Singletons.Sigma
import Data.Text.Prettyprint.Doc as D
import Data.Word
import Numeric

import Data.Elf
import Data.Elf.Exception
import Data.Elf.Headers
import Data.Interval

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
padLeadingZeros n s | otherwise = "0x" ++ replicate (n - length s) '0' ++ s

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
        , ("Flags",    printWord32 pFlags    ) -- Word32
        , ("Offset",   printWordXX pOffset   ) -- WordXX c
        , ("VirtAddr", printWordXX pVirtAddr ) -- WordXX c
        , ("PhysAddr", printWordXX pPhysAddr ) -- WordXX c
        , ("FileSize", printWordXX pFileSize ) -- WordXX c
        , ("MemSize",  printWordXX pMemSize  ) -- WordXX c
        , ("Align",    printWordXX pAlign    ) -- WordXX c
        ]

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
        padL n s | otherwise = replicate (n - length s) ' ' ++ s

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
                              , viaShow $ splitBits $ ElfSegmentFlag $ fromIntegral pFlags
                              ]
                    in
                        if empty i
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
                                    [(o,         b, doc)] ++
                                    xsf                         ++
                                    [(o + s - 1, e, [])]
                f RBuilderRawData{} =
                    let
                        doc = [ "R" ]
                    in
                        [(o,         "╓", doc)
                        ,(o + s - 1, "╙", [])
                        ]

--------------------------------------------------------------------
--
--------------------------------------------------------------------

formatPairsBlock :: Doc a -> [(String, Doc a)] -> Doc a
formatPairsBlock name pairs = vsep [ name <+> "{", indent 4 $ formatPairs pairs, "}" ]

printElfSymbolTableEntry :: SingI a => ElfSymbolTableEntry a -> Doc ()
printElfSymbolTableEntry ElfSymbolTableEntry{..} =
    formatPairsBlock ("symbol" <+> (dquotes $ pretty steName))
        [ ("Bind",  viaShow steBind      ) -- ElfSymbolBinding
        , ("Type",  viaShow steType      ) -- ElfSymbolType
        , ("ShNdx", viaShow steShNdx     ) -- ElfSectionIndex
        , ("Value", printWordXX steValue ) -- WordXX c
        , ("Size",  printWordXX steSize  ) -- WordXX c
        ]

printElfSymbolTable :: SingI a => [ElfSymbolTableEntry a] -> Doc ()
printElfSymbolTable l = align . vsep $
    case l of
        (e1 : e2 : _ : _) -> [ printElfSymbolTableEntry e1
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
        f s | otherwise  = Just $ BSL.splitAt n s

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

printData :: BSL.ByteString -> Doc ()
printData bs = align $ vsep $
    case splitBy 16 bs of
        (c1 : c2 : _ : _) -> [ formatBytestringLine c1
                             , formatBytestringLine c2
                             , "..."
                             , formatBytestringLine cl
                             , "total:" <+> viaShow (BSL.length bs)
                             ]
        chunks -> L.map formatBytestringLine chunks
    where
        cl = BSL.drop (BSL.length bs - 16) bs

printElf :: MonadThrow m => Sigma ElfClass (TyCon1 ElfList) -> m (Doc ())
printElf (classS :&: ElfList elfs) = withSingI classS do

    hData' <- do
        header <- elfFindHeader elfs
        case header of
            ElfHeader{..} -> return ehData
            _ -> $elfError "not a header" -- FIXME

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
        printElf'' s@ElfSection{ esData = (ElfSectionData bs), ..} =
            if sectionIsSymbolTable esType
                then do
                    stes <- parseSymbolTable hData' s elfs
                    return $ formatPairsBlock ("symbol table section" <+> (viaShow esN) <+> (dquotes $ pretty esName))
                        [ ("Type",       viaShow esType       )
                        , ("Flags",      printWordXX esFlags     )
                        , ("Data",       if null stes then "" else line <> (indent 4 $ printElfSymbolTable stes) )
                        ]
                else
                    return $ formatPairsBlock ("section" <+> (viaShow esN) <+> (dquotes $ pretty esName))
                        [ ("Type",       viaShow esType          )
                        , ("Flags",      printWordXX esFlags     )
                        , ("Addr",       printWordXX esAddr      )
                        , ("AddrAlign",  printWordXX esAddrAlign )
                        , ("EntSize",    printWordXX esEntSize   )
                        , ("Data",       printData bs            )
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
                , ("Flags",      printWord32 epFlags    )
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
                [ ("Data",       printData erData)
                ]

    printElf' elfs

--------------------------------------------------------------------
--
--------------------------------------------------------------------

printStringTable :: MonadThrow m => BSL.ByteString -> m (Doc ())
printStringTable bs =
    case BSL.unsnoc bs of
        Nothing -> return ""
        Just (bs', e) -> do
            when (e /= 0) $ $elfError "string table should end with 0"
            return if (BSL.length bs' == 0)
                then angles ""
                else vsep $ map (angles . pretty) $ L.sort $ map BSL8.unpack $ BSL.splitWith (== 0) $ bs'
