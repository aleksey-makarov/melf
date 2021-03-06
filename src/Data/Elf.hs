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

import Control.Exception.ChainedException
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

-- import System.IO.Unsafe

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
    | RBuilderRawAlign
        { rbraOffset :: WordXX c
        , rbraAlign  :: WordXX c
        }

rBuilderInterval :: IsElfClass a => RBuilder a -> Interval (WordXX a)
rBuilderInterval RBuilderHeader{..}       = headerInterval rbhHeader
rBuilderInterval RBuilderSectionTable{..} = sectionTableInterval rbstHeader
rBuilderInterval RBuilderSegmentTable{..} = segmentTableInterval rbptHeader
rBuilderInterval RBuilderSection{..}      = sectionInterval rbsHeader
rBuilderInterval RBuilderSegment{..}      = segmentInterval rbpHeader
rBuilderInterval RBuilderRawData{..}      = rbrdInterval
rBuilderInterval RBuilderRawAlign{}       = undefined -- FIXME

data LZip a = LZip [a] (Maybe a) [a]

instance Foldable LZip where
    foldMap f (LZip l  (Just c) r) = foldMap f $ LZip l Nothing (c : r)
    foldMap f (LZip l  Nothing  r) = foldMap f $ (L.reverse l) ++ r

findInterval :: (Ord t, Num t) => (a -> Interval t) -> t -> [a] -> LZip a
findInterval f e list = findInterval' [] list
    where
        findInterval' l []                           = LZip l Nothing []
        findInterval' l (x : xs) | e `touches` (f x) = LZip l (Just x) xs
                                 | e < offset  (f x) = LZip l Nothing (x : xs)
                                 | otherwise         = findInterval' (x : l) xs
        touches a i | I.empty i = a == offset i
                    | otherwise = a `member` i

showRBuilder' :: RBuilder a -> String
showRBuilder' RBuilderHeader{}       = "header"
showRBuilder' RBuilderSectionTable{} = "section table"
showRBuilder' RBuilderSegmentTable{} = "segment table"
showRBuilder' RBuilderSection{..}    = "section " ++ show rbsN
showRBuilder' RBuilderSegment{..}    = "segment " ++ show rbpN
showRBuilder' RBuilderRawData{}      = "raw data" -- should not be called
showRBuilder' RBuilderRawAlign{}     = "alignment" -- should not be called

showRBuilder :: IsElfClass a => RBuilder a -> String
showRBuilder v = showRBuilder' v ++ " (" ++ (show $ rBuilderInterval v) ++ ")"

-- showERBList :: IsElfClass a => [RBuilder a] -> String
-- showERBList l = "[" ++ (L.concat $ L.intersperse ", " $ fmap showRBuilder l) ++ "]"

intersectMessage :: IsElfClass a => RBuilder a -> RBuilder a -> String
intersectMessage x y = showRBuilder x ++ " and " ++ showRBuilder y ++ " intersect"

addRBuilders :: forall a m . (IsElfClass a, MonadCatch m) => [RBuilder a] -> m [RBuilder a]
addRBuilders newts =
    let
        addRBuilders' f newts' l = foldM (flip f) l newts'

        addRBuilderEmpty :: (IsElfClass a, MonadCatch m) => RBuilder a -> [RBuilder a] -> m [RBuilder a]
        addRBuilderEmpty t ts =
            -- (unsafePerformIO $ Prelude.putStrLn $ "Add Empty " ++ showRBuilder t ++ " to " ++ showERBList ts) `seq`
            let
                to = offset $ rBuilderInterval t
                (LZip l c' r) = findInterval rBuilderInterval to ts

                -- Let `(le, lo)` is the result of `allEmptyStarting a l`.
                -- Then `le` is the initial sublist of `l` each element of which is empty and starts at `a`,
                -- `lo` is the rest of `l`.
                allEmptyStartingAt :: WordXX a -> [RBuilder a] -> ([RBuilder a], [RBuilder a])
                allEmptyStartingAt a ls = f ([], ls)
                    where
                        f (le, []) = (L.reverse le, [])
                        f (le, h : lo) =
                            let
                                hi = rBuilderInterval h
                            in if (not $ I.empty hi) || (offset hi /= a)
                                then (L.reverse le, h : lo)
                                else f (h : le, lo)
            in case c' of
                Just RBuilderSegment{..} -> do
                    d <- $addContext' $ addRBuilderEmpty t rbpData
                    return $ toList $ LZip l (Just RBuilderSegment{ rbpData = d, .. }) r
                Just c ->
                    if offset (rBuilderInterval c) /= to then
                        $chainedError $ intersectMessage t c
                    else
                        let
                            (ce, re) = allEmptyStartingAt to (c : r)
                        in case t of
                            RBuilderSegment{..} ->
                                return $ toList $ LZip l (Just RBuilderSegment{ rbpData = ce, .. }) re
                            _ ->
                                return $ toList $ LZip l Nothing (ce ++ (t : re))
                Nothing -> return $ toList $ LZip l (Just t) r

        addRBuilderNonEmpty :: (IsElfClass a, MonadCatch m) => RBuilder a -> [RBuilder a] -> m [RBuilder a]
        addRBuilderNonEmpty t ts =
            -- (unsafePerformIO $ Prelude.putStrLn $ "Add NonEmpty " ++ showRBuilder t ++ " to " ++ showERBList ts) `seq`
            let
                ti = rBuilderInterval t
                (LZip l c' r) = findInterval rBuilderInterval (offset ti) ts

                addRBuildersNonEmpty :: (IsElfClass a, MonadCatch m) => [RBuilder a] -> RBuilder a -> m (RBuilder a)
                addRBuildersNonEmpty [] x = return x
                addRBuildersNonEmpty ts' RBuilderSegment{..} = do
                    d <- $addContext' $ addRBuilders' addRBuilderNonEmpty ts' rbpData
                    return RBuilderSegment{ rbpData = d, .. }
                addRBuildersNonEmpty (x:_) y = $chainedError $ intersectMessage x y

            in case c' of

                Just c ->

                    if ti == rBuilderInterval c then

                        case t of

                                -- NB: If a segment A has number greater than segment B and they have same size, then
                                --     segment A contains segment B
                                --     This should be taken into account in the serialization code.
                                RBuilderSegment{..} ->

                                    return $ toList $ LZip l (Just RBuilderSegment{ rbpData = [c], .. }) r

                                _ ->  do

                                    c'' <- $addContext' $ addRBuildersNonEmpty [t] c
                                    return $ toList $ LZip l (Just c'') r

                    else if rBuilderInterval c `contains` ti then do

                        c'' <- $addContext' $ addRBuildersNonEmpty [t] c
                        return $ toList $ LZip l (Just c'') r

                    else if ti `contains` rBuilderInterval c then

                        let

                            tir = offset ti + size ti - 1
                            (LZip l2 c2' r2) = findInterval rBuilderInterval tir r

                        in case c2' of

                            Nothing -> do

                                -- add this:     ......[t__________________________]...................
                                -- to this list: ......[c__]......[l2__]...[l2__].....[________].......
                                -- no need to keep the order of l2 as each member of the list will be placed independently from scratch
                                c'' <- $addContext' $ addRBuildersNonEmpty (c : l2) t
                                return $ toList $ LZip l (Just c'') r2

                            Just c2 ->

                                if ti `contains` (rBuilderInterval c2) then do

                                    -- add this:     ......[t______________________]........................
                                    -- to this list: ......[c_________]......[c2___]......[________]........
                                    c'' <- $addContext' $ addRBuildersNonEmpty (c : c2 : l2) t
                                    return $ toList $ LZip l (Just c'') r2
                                else

                                    -- add this:     ......[t_________________].............................
                                    -- to this list: ......[c_________]......[c2___]......[________]........
                                    $chainedError $ intersectMessage t c2

                    else

                        -- add this:     ..........[t________].............................
                        -- to this list: ......[c_________]......[_____]......[________]...
                        $chainedError $ intersectMessage t c

                Nothing ->

                    let
                        tir = offset ti + size ti - 1
                        (LZip l2 c2' r2) = findInterval rBuilderInterval tir r
                    in case c2' of

                        Nothing -> do

                            -- add this:     ....[t___].........................................
                            -- or this:      ....[t_________________________]...................
                            -- to this list: .............[l2__]...[l2__].....[________]........
                            c'' <- $addContext' $ addRBuildersNonEmpty l2 t
                            return $ toList $ LZip l (Just c'') r2

                        Just c2 ->

                            if ti `contains` (rBuilderInterval c2) then do

                                -- add this:     ....[t_________________________________]........
                                -- to this list: ..........[l2__]..[l2__].....[c2_______]........
                                c'' <- $addContext' $ addRBuildersNonEmpty (c2 : l2) t
                                return $ toList $ LZip l (Just c'') r2

                            else

                                -- add this:     ....[t_______________________________]..........
                                -- to this list: ..........[l2__]..[l2__].....[c2_______]........
                                $chainedError $ intersectMessage t c2

        (emptyRBs, nonEmptyRBs) = L.partition (I.empty . rBuilderInterval) newts

    in
        addRBuilders' addRBuilderNonEmpty nonEmptyRBs [] >>= addRBuilders' addRBuilderEmpty emptyRBs

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
        , esFlags     :: ElfSectionFlag
        , esAddr      :: WordXX c
        , esAddrAlign :: WordXX c
        , esEntSize   :: WordXX c
        , esN         :: Word16 -- FIXME: why these types differ?
        , esInfo      :: Word32
        , esLink      :: Word32 --
        , esData      :: ElfSectionData
        }
    | ElfSegment
        { epType     :: ElfSegmentType
        , epFlags    :: ElfSegmentFlag
        , epVirtAddr :: WordXX c
        , epPhysAddr :: WordXX c
        , epMemSize  :: WordXX c
        , epAlign    :: WordXX c
        , epData     :: [Elf c]
        }
    | ElfRawData
        { erData :: BSL.ByteString
        }
    | ElfRawAlign
        { raOffset :: WordXX c
        , raAlign  :: WordXX c
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
    then $chainedError "no section 0"
    else maybe ($chainedError $ "no section " ++ show n) return maybeSection
        where
            maybeSection = getFirst $ foldMapElfList f elfs
            f s@ElfSection{..} | esN == fromIntegral n = First $ Just s
            f _ = First Nothing

elfFindHeader :: forall a m . (SingI a, MonadThrow m) => [Elf a] -> m (Elf a)
elfFindHeader elfs = maybe ($chainedError $ "no header") return maybeHeader
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

nextOffset :: IsElfClass a => WordXX a -> WordXX a -> WordXX a -> WordXX a
nextOffset _ 0 a = a
nextOffset t m a | m .&. (m - 1) /= 0 = error $ "align module is not power of two " ++ (show m)
                 | otherwise          = if a' + t' < a then a' + m + t' else a' + t'
    where
        a' = a .&. complement (m - 1)
        t' = t .&. (m - 1)

addRawData :: forall a . IsElfClass a => BSL.ByteString -> [RBuilder a] -> [RBuilder a]
addRawData _ [] = []
addRawData bs rBuilders = snd $ addRawData' 0 (lrbie, rBuilders)
    where

        -- e, e', ee and lrbie stand for the first occupied byte after the place being fixed
        -- lrbi: last rBuilder interval (begin, size)
        lrbi@(I lrbib lrbis) = rBuilderInterval $ L.last rBuilders
        lrbie = if I.empty lrbi then lrbib else lrbib + lrbis

        allEmpty :: WordXX a -> WordXX a -> Bool
        allEmpty b s = BSL.all (== 0) bs'
            where
                bs' = cut bs (fromIntegral b) (fromIntegral s)

        addRawData' :: WordXX a -> (WordXX a, [RBuilder a]) -> (WordXX a, [RBuilder a])
        addRawData' alignHint (e, rbs) = L.foldr f (e, []) $ fmap fixRBuilder rbs
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
                fixRBuilder p@RBuilderSegment{..} =
                    RBuilderSegment{ rbpData = addRaw b ee' rbs', ..}
                        where
                            (I b s) = rBuilderInterval p
                            ee = b + s
                            alignHint' = max (pAlign rbpHeader) alignHint
                            (ee', rbs') = addRawData' alignHint' (ee, rbpData)
                fixRBuilder x = x

                -- b is the first free byte
                addRaw :: WordXX a -> WordXX a -> [RBuilder a] -> [RBuilder a]
                addRaw b ee rbs' =
                    if b < ee
                        then
                            if not $ allEmpty b s
                                then
                                    RBuilderRawData (I b s) : rbs'
                                else
                                    -- check e' < ee means
                                    -- check if next section/segment was actually placed (ee) with greater offset
                                    -- than is required by alignment rules (e')
                                    if e' < ee && e'' == ee
                                        then
                                            RBuilderRawAlign ee alignHint : rbs'
                                        else
                                            rbs'
                        else
                            rbs'
                    where
                        s = ee - b
                        eAddr = case rbs' of
                            (RBuilderSegment{rbpHeader = SegmentXX{..}} : _) -> pVirtAddr
                            _ -> 0
                        eAddrAlign = case rbs' of
                            (RBuilderSegment{rbpHeader = SegmentXX{..}} : _) -> pAlign
                            (RBuilderSection{rbsHeader = SectionXX{..}} : _) -> sAddrAlign
                            _ -> wordAlign $ fromSing $ sing @a
                        -- e' here is the address of the next section/segment
                        -- according to the regular alignment rules
                        e' = nextOffset eAddr eAddrAlign b
                        e'' = nextOffset ee alignHint b

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

    rbs <- addRBuilders $ [header] ++ maybeToList maybeSectionTable
                                   ++ maybeToList maybeSegmentTable
                                   ++ segments
                                   ++ sections
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
                , esFlags     = fromIntegral sFlags
                , esAddr      = sAddr
                , esAddrAlign = sAddrAlign
                , esEntSize   = sEntSize
                , esN         = rbsN
                , esInfo      = sInfo
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
        rBuilderToElf RBuilderRawAlign{..} =
            return $ ElfRawAlign rbraOffset rbraAlign

    el <- mapM rBuilderToElf rbs
    return $ sing :&: ElfList el

parseElf :: MonadCatch m => BSL.ByteString -> m Elf'
parseElf bs = do
    classS :&: HeadersXX (hdr, ss, ps) <- parseHeaders bs
    (withElfClass classS parseElf') hdr ss ps bs

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

data WBuilderData
    = WBuilderDataHeader
    | WBuilderDataByteStream { wbdData :: BSL.ByteString }
    | WBuilderDataSectionTable
    | WBuilderDataSegmentTable

data WBuilderState (a :: ElfClass) =
    WBuilderState
        { wbsSections         :: [(Word16, SectionXX a)]
        , wbsSegmentsReversed :: [SegmentXX a]
        , wbsDataReversed     :: [WBuilderData]
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
            _ -> $chainedError "not a header" -- FIXME

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
        align t m WBuilderState{..} | m .&. (m - 1) /= 0 = $chainedError $ "align module is not power of two " ++ (show m)
                                    | otherwise =
            let
                wbsOffset' = nextOffset t m wbsOffset
                d = WBuilderDataByteStream $ BSL.replicate (fromIntegral $ wbsOffset' - wbsOffset) 0
            in
                return WBuilderState
                    { wbsDataReversed = d : wbsDataReversed
                    , wbsOffset = wbsOffset'
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
        elf2WBuilder' ElfSection{esFlags = ElfSectionFlag f, ..} s = do
            when (f .&. (fromIntegral $ complement (maxBound @ (WordXX a))) /= 0)
                ($chainedError $ "section flags at section " ++ show esN ++ "don't fit")
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
                sFlags = fromIntegral f
                sAddr = esAddr                         -- WXX c
                sOffset = wbsOffset                    -- WXX c
                sSize = fromIntegral $ BSL.length d    -- WXX c
                sLink = esLink                         -- Word32
                sInfo = esInfo                         -- Word32
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
        elf2WBuilder' ElfRawAlign{..} s = align raOffset raAlign s

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

            when (not checkNeibours) ($chainedError "sections are not consistent")
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

-- FIXME: instance Binary Elf'

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
        _ -> $chainedError "not a section" -- FIXME
parseSymbolTable _ _ _ = $chainedError "incorrect args to parseSymbolTable" -- FIXME

-- FIXME: serializeSymbolTable
