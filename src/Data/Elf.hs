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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data.Elf is a module for parsing a ByteString of an ELF file into an Elf record.
module Data.Elf
    ( module Data.Elf.Generated
    , ElfSectionData (..)
    , Elf (..)
    , ElfList (..)
    , Elf'
    , RBuilder (..)
    , parseElf
    , parseRBuilder
    , getSectionData
    , getString
    , elfFindSection
    , elfFindHeader
    , rBuilderInterval
    , serializeElf'
    , serializeElf

    , ElfSymbolTableEntry(..)
    , parseSymbolTable
    ) where

import Data.Elf.Exception
import Data.Elf.Generated
import Data.Elf.Headers
import Data.Interval as I

import Control.Monad
import Control.Monad.Catch
import Control.Monad.State as MS
-- import Data.Bifunctor
import Data.Binary
import Data.Bits as Bin
import Data.ByteString.Lazy.Char8 as BSL8
import Data.ByteString.Lazy as BSL
-- import Data.Either
import Data.Foldable
import Data.Int
-- import Data.Kind
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import Data.Singletons
import Data.Singletons.Sigma
-- import Data.Word

headerInterval :: forall a . IsElfClass a => HeaderXX a -> Interval (WordXX a)
headerInterval _ = I 0 $ headerSize $ fromSing $ sing @a

sectionTableInterval :: IsElfClass a => HeaderXX a -> Interval (WordXX a)
sectionTableInterval HeaderXX{..} = I hShOff $ fromIntegral $ hShEntSize * hShNum

segmentTableInterval :: IsElfClass a => HeaderXX a -> Interval (WordXX a)
segmentTableInterval HeaderXX{..} = I hPhOff $ fromIntegral $ hPhEntSize * hPhNum

sectionInterval :: IsElfClass a => SectionXX a -> Interval (WordXX a)
sectionInterval SectionXX{..} = I sOffset if (sType == SHT_NOBITS) then 0 else sSize

segmentInterval :: IsElfClass a => SegmentXX a -> Interval (WordXX a)
segmentInterval SegmentXX{..} = I pOffset pFileSize

data RBuilder (c :: ElfClass)
    = RBuilderHeader
        { rbhHeader :: HeaderXX c
        }
    | RBuilderSectionTable
        { rbstHeader :: HeaderXX c
        }
    | RBuilderSegmentTable
        { rbptHeader :: HeaderXX c
        }
    | RBuilderSection -- FIXME: (?) add section name string
        { rbsHeader :: SectionXX c
        , rbsN      :: Word16
        }
    | RBuilderSegment
        { rbpHeader :: SegmentXX c
        , rbpN      :: Word16
        , rbpData   :: [RBuilder c]
        }
    | RBuilderRawData
        { rbrdInterval :: Interval (WordXX c)
        }

rBuilderInterval :: IsElfClass a => RBuilder a -> Interval (WordXX a)
rBuilderInterval RBuilderHeader{..}       = headerInterval rbhHeader
rBuilderInterval RBuilderSectionTable{..} = sectionTableInterval rbstHeader
rBuilderInterval RBuilderSegmentTable{..} = segmentTableInterval rbptHeader
rBuilderInterval RBuilderSection{..}      = sectionInterval rbsHeader
rBuilderInterval RBuilderSegment{..}      = segmentInterval rbpHeader
rBuilderInterval RBuilderRawData{..}      = rbrdInterval

data LZip a = LZip [a] (Maybe a) [a]

instance Foldable LZip where
    foldMap f (LZip l  (Just c) r) = foldMap f $ LZip l Nothing (c : r)
    foldMap f (LZip l  Nothing  r) = foldMap f $ (L.reverse l) ++ r

findInterval :: (Ord t, Num t) => (a -> Interval t) -> t -> [a] -> LZip a
findInterval f e list = findInterval' [] list
    where
        findInterval' l []                          = LZip l Nothing []
        findInterval' l (x : xs) | e `member` (f x) = LZip l (Just x) xs
        findInterval' l (x : xs) | e < offset (f x) = LZip l Nothing (x : xs)
        findInterval' l (x : xs) | otherwise        = findInterval' (x : l) xs

showRBuilber' :: RBuilder a -> String
showRBuilber' RBuilderHeader{}       = "header"
showRBuilber' RBuilderSectionTable{} = "section table"
showRBuilber' RBuilderSegmentTable{} = "segment table"
showRBuilber' RBuilderSection{..}    = "section " ++ show rbsN
showRBuilber' RBuilderSegment{..}    = "segment " ++ show rbpN
showRBuilber' RBuilderRawData{}      = "raw data" -- should not be called

showRBuilber :: IsElfClass a => RBuilder a -> String
showRBuilber v = showRBuilber' v ++ " (" ++ (show $ rBuilderInterval v) ++ ")"

-- showERBList :: [ElfRBuilder a] -> String
-- showERBList l = "[" ++ (L.concat $ L.intersperse ", " $ fmap showElfRBuilber l) ++ "]"

intersectMessage :: IsElfClass a => RBuilder a -> RBuilder a -> String
intersectMessage x y = showRBuilber x ++ " and " ++ showRBuilber y ++ " intersect"

addRBuildersToList :: (IsElfClass a, MonadCatch m) => [RBuilder a] -> [RBuilder a] -> m [RBuilder a]
addRBuildersToList newts l = foldM (flip addRBuilder) l newts

addRBuilders :: (IsElfClass a, MonadCatch m) => [RBuilder a] -> RBuilder a -> m (RBuilder a)
addRBuilders [] x = return x
addRBuilders ts RBuilderSegment{..} = do
    d <- addRBuildersToList ts rbpData
    return RBuilderSegment{ rbpData = d, .. }
addRBuilders (x:_) y = $elfError $ intersectMessage x y

addOneRBuilder :: (IsElfClass a, MonadCatch m) => RBuilder a -> RBuilder a -> m (RBuilder a)
addOneRBuilder t@RBuilderSegment{..} c | rBuilderInterval t == rBuilderInterval c = do
    d <- addRBuilder c rbpData
    return RBuilderSegment{ rbpData = d, .. }
addOneRBuilder t RBuilderSegment{..} = do
    d <- addRBuilder t rbpData
    return RBuilderSegment{ rbpData = d, .. }
addOneRBuilder t c = $elfError $ intersectMessage t c

addRBuilder :: (IsElfClass a, MonadCatch m) => RBuilder a -> [RBuilder a] -> m [RBuilder a]
addRBuilder t ts =
    let
        ti  = rBuilderInterval t
        tir = if I.empty ti then offset ti else offset ti + size ti - 1
        (LZip l  c'  r ) = findInterval rBuilderInterval (offset ti) ts
        (LZip l2 c2' r2) = findInterval rBuilderInterval tir         r
    in
        case (c', c2') of
            (Just c, _)  ->
                let
                    ci = rBuilderInterval c
                in if ci `contains` ti then

                    if I.empty ti && offset ti == offset ci
                        then

                            return $ toList $ LZip (t : l) c' r

                        else do

                            -- add this:     .........[t____].................................
                            -- or this:      .....[t___________]..............................
                            -- to this list: .....[c___________]......[___]......[________]...
                            c'' <- $addContext' $ addOneRBuilder t c
                            return $ toList $ LZip l (Just c'') r

                else if ti `contains` ci then
                    case c2' of

                        Nothing -> do

                            -- add this:     ......[t__________________________]...................
                            -- to this list: ......[c__]......[l2__]...[l2__].....[________].......
                            c'' <- $addContext' $ addRBuilders (c : l2) t
                            return $ toList $ LZip l (Just c'') r2

                        Just c2 ->
                            let
                                c2i = rBuilderInterval c2
                            in if ti `contains` c2i then do

                                -- add this:     ......[t______________________]........................
                                -- to this list: ......[c_________]......[c2___]......[________]........
                                c'' <- $addContext' $ addRBuilders (c : l2 ++ [c2]) t
                                return $ toList $ LZip l (Just c'') r2
                            else

                                -- add this:     ......[t_________________].............................
                                -- to this list: ......[c_________]......[c2___]......[________]........
                                $elfError $ intersectMessage t c2
                else

                    -- add this:     ..........[t________].............................
                    -- to this list: ......[c_________]......[_____]......[________]...
                    $elfError $ intersectMessage t c

            (Nothing, Nothing) -> do

                -- add this:     ....[t___].........................................
                -- or this:      ....[t_________________________]...................
                -- to this list: .............[l2__]...[l2__].....[________]........
                c'' <- $addContext' $ addRBuilders l2 t
                return $ toList $ LZip l (Just c'') r2

            (Nothing, Just c2) ->
                let
                    c2i = rBuilderInterval c2
                in if ti `contains` c2i then do

                    -- add this:     ....[t_________________________________]........
                    -- to this list: ..........[l2__]..[l2__].....[c2_______]........
                    c'' <- $addContext' $ addRBuilders (l2 ++ [c2]) t
                    return $ toList $ LZip l (Just c'') r2

                else

                    -- add this:     ....[t_______________________________]..........
                    -- to this list: ..........[l2__]..[l2__].....[c2_______]........
                    $elfError $ intersectMessage t c2

data ElfSectionData
    = ElfSectionData BSL.ByteString
    | ElfSectionDataStringTable

data Elf (c :: ElfClass)
    = ElfHeader
        { ehData       :: ElfData
        , ehOSABI      :: ElfOSABI
        , ehABIVersion :: Word8
        , ehType       :: ElfType
        , ehMachine    :: ElfMachine
        , ehEntry      :: WordXX c
        , ehFlags      :: Word32
        }
    | ElfSectionTable
    | ElfSegmentTable
    | ElfSection
        { esName      :: String -- NB: different
        , esType      :: ElfSectionType
        , esFlags     :: WordXX c
        , esAddr      :: WordXX c
        , esAddrAlign :: WordXX c
        , esEntSize   :: WordXX c
        , esN         :: Word16
        , esLink      :: Word32
        , esData      :: ElfSectionData
        }
    | ElfSegment
        { epType     :: ElfSegmentType
        , epFlags    :: Word32
        , epVirtAddr :: WordXX c
        , epPhysAddr :: WordXX c
        , epMemSize  :: WordXX c
        , epAlign    :: WordXX c
        , epData     :: [Elf c]
        }
    | ElfRawData
        { erData :: BSL.ByteString
        }

-- FIXME MyTree nodeT leafT, bifunctor MyTree, Elf -> split to 6 separate types

-- FIXME: Write GADT record with constrained type: https://stackoverflow.com/questions/21505975/write-gadt-record-with-constrained-type
-- data ElfNodeType = Header | SectionTable | SegmentTable | Section | Segment | Raw
--
-- type Elf :: ElfClass -> ElfNodeType -> Type
-- data Elf c t where
--         ElfHeader ::
--             { ehData       :: ElfData
--             , ehOSABI      :: ElfOSABI
--             , ehABIVersion :: Word8
--             , ehType       :: ElfType
--             , ehMachine    :: ElfMachine
--             , ehEntry      :: WXX c
--             , ehFlags      :: Word32
--             } -> Elf c 'Header
--         ElfSectionTable :: Elf c 'SectionTable
--         ElfSegmentTable :: Elf c 'SegmentTable
--         ElfSection ::
--             { esName      :: String -- NB: different
--             , esType      :: ElfSectionType
--             , esFlags     :: WXX c
--             , esAddr      :: WXX c
--             , esAddrAlign :: WXX c
--             , esEntSize   :: WXX c
--             , esN         :: Word16
--             , esLink      :: Word32
--             , esData      :: ElfSectionData
--             } -> Elf c 'Section
--         ElfSegment ::
--             { epType     :: ElfSegmentType
--             , epFlags    :: Word32
--             , epVirtAddr :: WXX c
--             , epPhysAddr :: WXX c
--             , epMemSize  :: WXX c
--             , epAlign    :: WXX c
--             , epData     :: [Elf c t']
--             } -> Elf c 'Segment
--         ElfRawData ::
--             { erData :: BSL.ByteString
--             } -> Elf c 'Raw
--
-- -- FIXME: ElfSomeNode
-- type ElfNode :: ElfClass -> Type
-- data ElfNode c = forall t' . ElfNode { getElf :: Elf c t' }

foldMapElf :: Monoid m => (Elf a -> m) -> Elf a -> m
foldMapElf f e@ElfSegment{..} = f e <> foldMapElfList f epData
foldMapElf f e = f e

foldMapElfList :: Monoid m => (Elf a -> m) -> [Elf a] -> m
foldMapElfList f l = fold $ fmap (foldMapElf f) l

elfFindSection :: forall a m b . (SingI a, MonadThrow m, Integral b, Show b) => [Elf a] -> b -> m (Elf a)
elfFindSection elfs n = if n == 0
    then $elfError "no section 0"
    else maybe ($elfError $ "no section " ++ show n) return maybeSection
        where
            maybeSection = getFirst $ foldMapElfList f elfs
            f s@ElfSection{..} | esN == fromIntegral n = First $ Just s
            f _ = First Nothing

elfFindHeader :: forall a m . (SingI a, MonadThrow m) => [Elf a] -> m (Elf a)
elfFindHeader elfs = maybe ($elfError $ "no header") return maybeHeader
    where
        maybeHeader = getFirst $ foldMapElfList f elfs
        f h@ElfHeader{} = First $ Just h
        f _ = First Nothing

-- FIXME: Elf' should be just Elf
newtype ElfList c = ElfList [Elf c]
type Elf' = Sigma ElfClass (TyCon1 ElfList)

getString :: BSL.ByteString -> Int64 -> String
getString bs offset = BSL8.unpack $ BSL.takeWhile (/= 0) $ BSL.drop offset bs

cut :: BSL.ByteString -> Int64 -> Int64 -> BSL.ByteString
cut content offset size = BSL.take size $ BSL.drop offset content

getSectionData :: IsElfClass a => BSL.ByteString -> SectionXX a -> BSL.ByteString
getSectionData bs SectionXX{..} = cut bs o s
    where
        o = fromIntegral sOffset
        s = fromIntegral sSize

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : xs) = xs

addRawData :: forall a . IsElfClass a => BSL.ByteString -> [RBuilder a] -> [RBuilder a]
addRawData _ [] = []
addRawData bs rBuilders = snd $ addRawData' (lrbie, rBuilders)
    where

        allEmpty :: WordXX a -> WordXX a -> Bool
        allEmpty b s = BSL.all (== 0) bs'
            where
                bs' = cut bs (fromIntegral b) (fromIntegral s)

        -- e, e' and lrbie stand for the first occupied byte after the place being fixed

        -- names: last rBuilder interval (begin, size)
        lrbi@(I lrbib lrbis) = rBuilderInterval $ L.last rBuilders
        lrbie = if I.empty lrbi then lrbib else lrbib + lrbis

        addRaw :: WordXX a -> WordXX a -> [RBuilder a] -> [RBuilder a]
        addRaw b e rbs =
            if b < e && (not $ allEmpty b s)
                then RBuilderRawData (I b s) : rbs
                else rbs
            where
                s = e - b

        addRawData' :: (WordXX a, [RBuilder a]) -> (WordXX a, [RBuilder a])
        addRawData' (e, rbs) = L.foldr f (e, []) $ fmap fixRBuilder rbs
            where
                f rb (e', rbs') =
                    let
                        i@(I b s) = rBuilderInterval rb
                        b' = if I.empty i then b else b + s
                        rbs'' = addRaw b' e' rbs'
                    in
                        (b, rb : rbs'')

        fixRBuilder :: RBuilder a -> RBuilder a
        fixRBuilder p | I.empty $ rBuilderInterval p = p
        fixRBuilder p@RBuilderSegment{..}            = RBuilderSegment{ rbpData = addRaw b e' rbs, ..}
            where
                (I b s) = rBuilderInterval p
                e = b + s
                (e', rbs) = addRawData' (e, rbpData)
        fixRBuilder x = x

parseRBuilder :: (IsElfClass a, MonadCatch m) => HeaderXX a -> [SectionXX a] -> [SegmentXX a] -> BSL.ByteString -> m [RBuilder a]
parseRBuilder hdr@HeaderXX{..} ss ps bs = do

    let
        mkRBuilderSection :: (SingI a, MonadCatch m) => (Word16, SectionXX a) -> m (RBuilder a)
        mkRBuilderSection (n, s) = return $ RBuilderSection s n

        mkRBuilderSegment :: (SingI a, MonadCatch m) => (Word16, SegmentXX a) -> m (RBuilder a)
        mkRBuilderSegment (n, s) = return $ RBuilderSegment s n []

    sections <- mapM mkRBuilderSection $ tail' $ Prelude.zip [0 .. ] ss
    segments <- mapM mkRBuilderSegment $         Prelude.zip [0 .. ] ps

    let

        header            = RBuilderHeader hdr
        maybeSectionTable = if hShNum == 0 then Nothing else  Just $ RBuilderSectionTable hdr
        maybeSegmentTable = if hPhNum == 0 then Nothing else  Just $ RBuilderSegmentTable hdr

    rbs <- addRBuilder header
        =<< maybe return addRBuilder maybeSectionTable
        =<< maybe return addRBuilder maybeSegmentTable
        =<< addRBuildersToList sections
        =<< addRBuildersToList segments []

    return $ addRawData bs rbs

parseElf' :: forall a m . (IsElfClass a, MonadCatch m) =>
                                            HeaderXX a ->
                                         [SectionXX a] ->
                                         [SegmentXX a] ->
                                        BSL.ByteString -> m (Elf')
parseElf' hdr@HeaderXX{..} ss ps bs = do

    rbs <- parseRBuilder hdr ss ps bs

    let
        firstJust f = listToMaybe . mapMaybe f
        isStringTable (n, s) | n == hShStrNdx = Just $ getSectionData bs s
        isStringTable _                       = Nothing
        maybeStringData = firstJust isStringTable $ tail' $ Prelude.zip [0 .. ] ss
        stringData = maybe BSL.empty id maybeStringData

        rBuilderToElf :: RBuilder a -> m (Elf a)
        rBuilderToElf RBuilderHeader{} =
            return ElfHeader
                { ehData       = hData
                , ehOSABI      = hOSABI
                , ehABIVersion = hABIVersion
                , ehType       = hType
                , ehMachine    = hMachine
                , ehEntry      = hEntry
                , ehFlags      = hFlags
                }
        rBuilderToElf RBuilderSectionTable{} =
            return ElfSectionTable
        rBuilderToElf RBuilderSegmentTable{} =
            return ElfSegmentTable
        rBuilderToElf RBuilderSection{ rbsHeader = s@SectionXX{..}, ..} =
            return ElfSection
                { esName      = getString stringData $ fromIntegral sName
                , esType      = sType
                , esFlags     = sFlags
                , esAddr      = sAddr
                , esAddrAlign = sAddrAlign
                , esEntSize   = sEntSize
                , esN         = rbsN
                , esLink      = sLink
                , esData      = if rbsN == hShStrNdx
                    then
                        ElfSectionDataStringTable
                    else
                        ElfSectionData if I.empty $ sectionInterval s
                            then BSL.empty
                            else getSectionData bs s
                }
        rBuilderToElf RBuilderSegment{ rbpHeader = SegmentXX{..}, ..} = do
            d <- mapM rBuilderToElf rbpData
            return ElfSegment
                { epType     = pType
                , epFlags    = pFlags
                , epVirtAddr = pVirtAddr
                , epPhysAddr = pPhysAddr
                , epMemSize  = pMemSize
                , epAlign    = pAlign
                , epData     = d
                }
        rBuilderToElf RBuilderRawData{ rbrdInterval = I o s } =
            return $ ElfRawData $ cut bs (fromIntegral o) (fromIntegral s)

    el <- mapM rBuilderToElf rbs
    return $ sing :&: ElfList el

parseElf :: MonadCatch m => BSL.ByteString -> m Elf'
parseElf bs = do
    classS :&: HeadersXX (hdr, ss, ps) <- parseHeaders bs
    (withElfClass classS parseElf') hdr ss ps bs

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

data WBuilderData (a :: ElfClass)
    = WBuilderDataHeader
    | WBuilderDataByteStream { wbdData :: BSL.ByteString }
    | WBuilderDataSectionTable
    | WBuilderDataSegmentTable

data WBuilderState (a :: ElfClass) =
    WBuilderState
        { wbsSections         :: [(Word16, SectionXX a)]
        , wbsSegmentsReversed :: [SegmentXX a]
        , wbsDataReversed     :: [WBuilderData a]
        , wbsOffset           :: WordXX a
        , wbsPhOff            :: WordXX a
        , wbsShOff            :: WordXX a
        , wbsShStrNdx         :: Word16
        , wbsNameIndexes      :: [Int64]
        }

wbStateInit :: forall a . IsElfClass a => WBuilderState a
wbStateInit = WBuilderState
    { wbsSections         = []
    , wbsSegmentsReversed = []
    , wbsDataReversed     = []
    , wbsOffset           = 0
    , wbsPhOff            = 0
    , wbsShOff            = 0
    , wbsShStrNdx         = 0
    , wbsNameIndexes      = []
    }

zeroSection :: forall a . IsElfClass a => SectionXX a
zeroSection = SectionXX 0 0 0 0 0 0 0 0 0 0

neighbours :: [a] -> (a -> a -> b) -> [b]
neighbours [] _ = []
neighbours x  f = fmap (uncurry f) $ L.zip x $ L.tail x

-- make string table and indexes for it from a list of strings
mkStringTable :: [String] -> (BSL.ByteString, [Int64])
mkStringTable sectionNames = (stringTable, os)
    where

        -- names:
        -- i for indexes of the section entry in section table
        -- n for section name string
        -- o for offset of the string in the string table
        -- in, io -- for pairs
        -- ins, ios -- for lists of pairs
        -- etc

        (ins0, ins) = L.break ((/= "") . snd) $ L.sortOn (L.length . snd) $ L.zip [(1 :: Word32) .. ] sectionNames
        ios0 = fmap f' ins0
            where
                f' (i, _) = (i, 0)

        (stringTable, ios, _) = f (BSL.singleton 0, [], L.reverse ins)

        os = fmap snd $ L.sortOn fst $ ios0 ++ ios

        -- create string table.  If one name is a suffix of another,
        -- allocate only the longest name in string table
        f x@(_, _, []) = x
        f (st, iosf, (i, n) : insf) = f (st', iosf'', insf')

            where

                st' = st <> BSL8.pack n <> BSL.singleton 0
                o = BSL.length st
                iosf'' = (i, o) : iosf' ++ iosf

                (iosf', insf') = ff insf

                -- look if there exists a name that is a suffix for the currently allocated name
                -- in the list of unallocated indexed section names
                ff = L.foldr fff ([], [])
                    where
                        fff (i', n') (iosff, insff) = if n' `L.isSuffixOf` n
                            then
                                let
                                    o' = o + fromIntegral (L.length n - L.length n')
                                in
                                    ((i', o') : iosff, insff)
                            else (iosff, (i', n') : insff)

-- FIXME: rewrite all this using lenses
serializeElf' :: forall a m . (IsElfClass a, MonadThrow m) => [Elf a] -> m BSL.ByteString
serializeElf' elfs = do

    (header', hData') <- do
        header <- elfFindHeader elfs
        case header of
            ElfHeader{..} -> return (header, ehData)
            _ -> $elfError "not a header" -- FIXME

    let

        elfClass = fromSing $ sing @a

        sectionN :: Num b => b
        sectionN = getSum $ foldMapElfList f elfs
            where
                f ElfSection{} = Sum 1
                f _ =  Sum 0

        sectionNames :: [String]
        sectionNames = foldMapElfList f elfs
            where
                f ElfSection{..} = [ esName ]
                f _ = []

        (stringTable, nameIndexes) = mkStringTable sectionNames

        segmentN :: Num b => b
        segmentN = getSum $ foldMapElfList f elfs
            where
                f ElfSegment{} = Sum 1
                f _ =  Sum 0

        sectionTable :: Bool
        sectionTable = getAny $ foldMapElfList f elfs
            where
                f ElfSectionTable =  Any True
                f _ = Any False

        align :: MonadThrow n => WordXX a -> WordXX a -> WBuilderState a -> n (WBuilderState a)
        align _ 0 x = return x
        align _ 1 x = return x
        align t m WBuilderState{..} | m .&. (m - 1) /= 0 = $elfError $ "align module is not power of two " ++ (show m)
                                    | otherwise =
            let
                o' = wbsOffset .&. complement (m - 1)
                t' = t .&. (m - 1)
                o'' = if o' + t' < wbsOffset
                    then o' + m + t'
                    else o' + t'
                d = WBuilderDataByteStream $ BSL.replicate (fromIntegral $ o'' - wbsOffset) 0
            in
                return WBuilderState
                    { wbsDataReversed = d : wbsDataReversed
                    , wbsOffset = o''
                    , ..
                    }

        alignWord :: MonadThrow n => WBuilderState a -> n (WBuilderState a)
        alignWord = align 0 $ wordAlign $ fromSing $ sing @a

        dataIsEmpty :: ElfSectionData -> Bool
        dataIsEmpty (ElfSectionData bs)       = BSL.null bs
        dataIsEmpty ElfSectionDataStringTable = BSL.null stringTable

        lastSectionIsEmpty :: [Elf a] -> Bool
        lastSectionIsEmpty [] = False
        lastSectionIsEmpty l = case L.last l of
            ElfSection{..} -> esType == SHT_NOBITS || dataIsEmpty esData
            _ -> False

        elf2WBuilder' :: MonadThrow n => Elf a -> WBuilderState a -> n (WBuilderState a)
        elf2WBuilder' ElfHeader{} WBuilderState{..} =
            return WBuilderState
                { wbsDataReversed = WBuilderDataHeader : wbsDataReversed
                , wbsOffset = wbsOffset + headerSize elfClass
                , ..
                }
        elf2WBuilder' ElfSectionTable s = do
            WBuilderState{..} <- alignWord s
            return WBuilderState
                { wbsDataReversed = WBuilderDataSectionTable : wbsDataReversed
                , wbsOffset = wbsOffset + (sectionN + 1) * sectionTableEntrySize elfClass
                , wbsShOff = wbsOffset
                , ..
                }
        elf2WBuilder' ElfSegmentTable s = do
            WBuilderState{..} <- alignWord s
            return WBuilderState
                { wbsDataReversed = WBuilderDataSegmentTable : wbsDataReversed
                , wbsOffset = wbsOffset + segmentN * segmentTableEntrySize elfClass
                , wbsPhOff = wbsOffset
                , ..
                }
        elf2WBuilder' ElfSection{..} s = do
            WBuilderState{..} <- if esType == SHT_NOBITS
                then return s
                else align 0 esAddrAlign s
            let
                (d, shStrNdx) = case esData of
                    ElfSectionData bs -> (bs, wbsShStrNdx)
                    ElfSectionDataStringTable -> (stringTable, esN)
                (n, ns) = case wbsNameIndexes of
                    n' : ns' -> (n', ns')
                    _ -> error "internal error: different number of sections in two iterations"
                sName = fromIntegral n                 -- Word32
                sType = esType                         -- ElfSectionType
                sFlags = esFlags                       -- WXX c
                sAddr = esAddr                         -- WXX c
                sOffset = wbsOffset                    -- WXX c
                sSize = fromIntegral $ BSL.length d    -- WXX c
                sLink = esLink                         -- Word32
                sInfo = 0                              -- Word32 FIXME
                sAddrAlign = esAddrAlign               -- WXX c
                sEntSize = esEntSize                   -- WXX c
            return WBuilderState
                { wbsSections = (esN, SectionXX{..}) : wbsSections
                , wbsDataReversed = (WBuilderDataByteStream d) : wbsDataReversed
                , wbsOffset = wbsOffset + (fromIntegral $ BSL.length d)
                , wbsShStrNdx = shStrNdx
                , wbsNameIndexes = ns
                , ..
                }
        elf2WBuilder' ElfSegment{..} s = do
            s' <- align epVirtAddr epAlign s
            let
                offset = wbsOffset s'
            WBuilderState{..} <- execStateT (mapM elf2WBuilder epData) s'
            let
                -- allocate one more byte in the end of segment if there exists an empty section
                -- at the end so that that empty section will go to the current segment
                add1 = lastSectionIsEmpty epData && offset /= wbsOffset
                pType = epType
                pFlags = epFlags
                pOffset = offset
                pVirtAddr = epVirtAddr
                pPhysAddr = epPhysAddr
                pFileSize = wbsOffset - offset + if add1 then 1 else 0
                pMemSize = epMemSize
                pAlign = epAlign
            return WBuilderState
                { wbsSegmentsReversed = SegmentXX{..} : wbsSegmentsReversed
                , wbsDataReversed = if add1
                    then (WBuilderDataByteStream $ BSL.singleton 0) : wbsDataReversed
                    else wbsDataReversed
                , wbsOffset = if add1
                    then wbsOffset + 1
                    else wbsOffset
                , ..
                }
        elf2WBuilder' ElfRawData{..} WBuilderState{..} =
            return WBuilderState
                { wbsDataReversed = (WBuilderDataByteStream erData) : wbsDataReversed
                , wbsOffset = wbsOffset + (fromIntegral $ BSL.length erData)
                , ..
                }

        elf2WBuilder :: (MonadThrow n, MonadState (WBuilderState a) n) => Elf a -> n ()
        elf2WBuilder elf = MS.get >>= elf2WBuilder' elf >>= MS.put

        fixSections :: [(Word16, SectionXX a)] -> m [SectionXX a]
        fixSections ss = do
            when (L.length ss /= sectionN) (error "internal error: L.length ss /= sectionN")
            let
                f (ln, _) (rn, _) = ln `compare` rn
                sorted = L.sortBy f ss
                next (ln, _) (rn, _) = ln + 1 == rn
                checkNeibours = L.all id $ neighbours sorted next

            when (not checkNeibours) ($elfError "sections are not consistent")
            return $ fmap snd sorted

        wbState2ByteString :: WBuilderState a -> m BSL.ByteString
        wbState2ByteString WBuilderState{..} = do

            sections <- fixSections wbsSections

            let
                f WBuilderDataHeader =
                    case header' of
                        ElfHeader{..} ->
                            let
                                hData       = ehData
                                hOSABI      = ehOSABI
                                hABIVersion = ehABIVersion
                                hType       = ehType
                                hMachine    = ehMachine
                                hEntry      = ehEntry
                                hPhOff      = wbsPhOff
                                hShOff      = wbsShOff
                                hFlags      = ehFlags
                                hPhEntSize  = segmentTableEntrySize elfClass
                                hPhNum      = segmentN
                                hShEntSize  = sectionTableEntrySize elfClass
                                hShNum      = if sectionTable then sectionN + 1 else 0
                                hShStrNdx   = wbsShStrNdx

                                h :: Header
                                h = sing @ a :&: HeaderXX{..}
                            in
                                encode h
                        _ -> error "this should be ElfHeader" -- FIXME
                f WBuilderDataByteStream {..} = wbdData
                f WBuilderDataSectionTable =
                    serializeListA hData' $ zeroSection : sections
                f WBuilderDataSegmentTable =
                    serializeListA hData' $ L.reverse wbsSegmentsReversed

            return $ foldMap f $ L.reverse wbsDataReversed

    execStateT (mapM elf2WBuilder elfs) wbStateInit{ wbsNameIndexes = nameIndexes } >>= wbState2ByteString

serializeElf :: MonadThrow m => Elf' -> m BSL.ByteString
serializeElf (classS :&: ElfList ls) = (withElfClass classS serializeElf') ls

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- FIXME: move this to a separate file

data ElfSymbolTableEntry (c :: ElfClass) =
    ElfSymbolTableEntry
        { steName  :: String -- NB: different
        , steBind  :: ElfSymbolBinding
        , steType  :: ElfSymbolType
        , steShNdx :: ElfSectionIndex
        , steValue :: WordXX c
        , steSize  :: WordXX c
        }

getStringFromData :: BSL.ByteString -> Word32 -> String
getStringFromData stringTable offset = BSL8.unpack $ BSL.takeWhile (/= 0) $ BSL.drop (fromIntegral offset) stringTable

mkElfSymbolTableEntry :: SingI a => BSL.ByteString -> SymbolTableEntryXX a -> ElfSymbolTableEntry a
mkElfSymbolTableEntry stringTable SymbolTableEntryXX{..} =
    let
        steName  = getStringFromData stringTable stName
        steBind  = ElfSymbolBinding $ stInfo `shiftR` 4
        steType  = ElfSymbolType $ stInfo .&. 0x0f
        steShNdx = stShNdx
        steValue = stValue
        steSize  = stSize
    in
        ElfSymbolTableEntry{..}

parseSymbolTable :: (MonadThrow m, SingI a) => ElfData -> Elf a -> [Elf a] -> m [ElfSymbolTableEntry a]
parseSymbolTable d ElfSection{ esData = ElfSectionData symbolTable, ..} elfs = do
    section <- elfFindSection elfs esLink
    case section of
        ElfSection{ esData = ElfSectionData stringTable } -> do
            st <- parseListA d symbolTable
            return (mkElfSymbolTableEntry stringTable <$> st)
        _ -> $elfError "not a section" -- FIXME
parseSymbolTable _ _ _ = $elfError "incorrect args to parseSymbolTable" -- FIXME

-- FIXME: serializeSymbolTable
