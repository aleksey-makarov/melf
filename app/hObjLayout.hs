{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Monad
import Data.Binary
import Data.Elf
import Data.List
import Data.Maybe
import Data.Semigroup
import Numeric.Interval as I
import Numeric.Interval.NonEmpty as INE
import System.Environment
import Text.Printf

data T = S (INE.Interval Word64) Word ElfSection [T]
       | P (INE.Interval Word64) Word ElfSegment [T]
       | H (INE.Interval Word64) String          [T]

instance Show T where
    show (S i n s _) = "(" ++ show n ++ ": [" ++ show i ++ "] " ++ (nameToString $ elfSectionName s) ++ ")"
    show (P i n p _) = "(" ++ show n ++ ": [" ++ show i ++ "] " ++ (show $ elfSegmentType p)         ++ ")"
    show (H i   s _) = "(" ++             "[" ++ show i ++ "] " ++ s                                 ++ ")"

class HasInterval a t | a -> t where
    getInterval :: a -> INE.Interval t

instance HasInterval T Word64 where
    getInterval (S i _ _ _) = i
    getInterval (P i _ _ _) = i
    getInterval (H i _ _)   = i

data LZip a = LZip [a] (Maybe a) [a]

findInterval :: (Ord t, HasInterval a t) => t -> [a] -> LZip a
findInterval e list = findInterval' list []
    where
        findInterval' []       l                                = LZip l Nothing []
        findInterval' (x : xs) l | INE.member e (getInterval x) = LZip l (Just x) xs
        findInterval' (x : xs) l | e < INE.inf (getInterval x)  = LZip l Nothing (x : xs)
        findInterval' (x : xs) l | otherwise                    = findInterval' xs (x : l)

foldInterval :: LZip a -> [a]
foldInterval (LZip l         (Just c) r) = foldInterval $ LZip l  Nothing (c : r)
foldInterval (LZip (l : ls)  Nothing  r) = foldInterval $ LZip ls Nothing (l : r)
foldInterval (LZip []        Nothing  r) = r

intersectMessage :: T -> T -> String
intersectMessage a b = show a ++ " and " ++ show b ++ " interlap"

addTs :: [T] -> T -> T
addTs ts (S i at s tl) = S i at s $ addTsToList ts tl
addTs ts (P i at p tl) = P i at p $ addTsToList ts tl
addTs ts (H i s    tl) = H i s    $ addTsToList ts tl

addT :: T -> [T] -> [T]
addT t ts =
    let
        ti = getInterval t
        i = INE.inf ti
        s = INE.sup ti
        (LZip l  c  r ) = findInterval i ts
        (LZip l2 c2 r2) = findInterval s r
    in
        case (c, c2) of
            (Just c', _)  ->
                let
                    c'i = getInterval c'
                in
                    if c'i `INE.contains` ti then
                        foldInterval $ LZip l (Just $ addTs [t] c') r
                    else  if ti `INE.contains` c'i then
                        case c2 of
                            Nothing -> foldInterval $ LZip l (Just $ addTs (c' : l2) t) r2
                            Just c2' -> error $ "@1 " ++ intersectMessage t c2'
                    else
                        error $ "@2 " ++ intersectMessage t c'
            (Nothing, Nothing)  -> foldInterval $ LZip l (Just $ addTs l2 t) r2
            (Nothing, Just c2') ->
                let
                    c2'i = getInterval c2'
                in
                    if ti `INE.contains` c2'i then
                        foldInterval $ LZip l (Just $ addTs (l2 ++ [c2']) t) r2
                    else
                        error $ "@3 " ++ intersectMessage t c2'

addTsToList :: [T] -> [T] -> [T]
addTsToList newTs l = foldl (flip addT) l newTs

printElf :: String -> IO ()
printElf fileName = do

    elf <- decodeFile fileName

    let
        ss = elfSections elf
        ps = elfSegments elf

        printSection :: (Word, ElfSection) -> IO ()
        printSection (i, s) = printf "\t- %-2d %s\n" i (nameToString $ elfSectionName s)

        printSegment :: (Word, ElfSegment) -> IO ()
        printSegment (i, p) = printf "\t- %-2d %s\n" i (show $ elfSegmentType p)

    putStrLn "sections:"
    mapM_ printSection $ zip [0 ..] ss
    putStrLn "segments:"
    mapM_ printSegment $ zip [0 ..] ps

    let

        toNonEmpty i | I.null i  = Nothing
        toNonEmpty i | otherwise = Just (I.inf i INE.... I.sup i)

        sti = Data.Elf.interval <$> elfSectionTableInterval elf
        pti = Data.Elf.interval <$> elfSegmentTableInterval elf

        headerIntervals = mapMaybe id [ Just $ H (elfHeaderInterval elf) "ELF Header" []
                                      , (\ x -> H x "Section table" []) <$> sti
                                      , (\ x -> H x "Segment table" []) <$> pti
                                      ]

        fSection :: (Word, ElfSection) -> Maybe T
        fSection (i, s) = (\ x -> S x i s []) <$> (toNonEmpty $ elfSectionInterval s)

        fSegment :: (Word, ElfSegment) -> Maybe T
        fSegment (i, p) = (\ x -> P x i p []) <$> (toNonEmpty $ elfSegmentInterval p)

        sectionIntervals = mapMaybe fSection $ zip [0 ..] ss
        segmentIntervals = mapMaybe fSegment $ zip [0 ..] ps

        intervals = addTsToList sectionIntervals $ addTsToList headerIntervals $ addTsToList segmentIntervals []

        tsDepth :: [T] -> Word
        tsDepth l = getMax $ foldr (<>) 0 $ fmap (Max . tDepth) l

        tDepth :: T -> Word
        tDepth (S _ _ _ l) = 1 + tsDepth l
        tDepth (P _ _ _ l) = 1 + tsDepth l
        tDepth (H _   _ l) = 1 + tsDepth l

        printHeader :: [Char] -> Word -> Char -> IO ()
        printHeader p d f = do
            mapM_ putChar $ reverse p
            let
                n = fromIntegral d - length p
            putChar f
            when (n > 1) do
                mapM_ putChar $ genericReplicate (n - 1) '─'

        printT :: [Char] -> Word -> T -> IO ()
        printT p w (S i at s tl) = do
            printHeader p w '┌'
            printf " %016x S %d name: \"%s\"; type: %s; flags: %s\n" (INE.inf i) at (nameToString $ elfSectionName s) (show $ elfSectionType s) (show $ splitBits $ elfSectionFlags s)
            mapM_ (printT ('│' : p) w) tl
            printHeader p w '└'
            printf " %016x S %d\n" (INE.sup i) at
        printT p w (P i at segment tl) = do
            printHeader p w '╓'
            printf " %016x P %d type: %s; flags: %s\n" (INE.inf i) at (show $ elfSegmentType segment) (show $ splitBits $ elfSegmentFlags segment)
            mapM_ (printT ('║' : p) w) tl
            printHeader p w '╙'
            printf " %016x P %d\n" (INE.sup i) at
        printT p w (H i s    tl) = do
            printHeader p w '┎'
            printf " %016x H %s\n" (INE.inf i) s
            mapM_ (printT ('┃' : p) w) tl
            printHeader p w '┖'
            printf " %016x H\n" (INE.sup i)

        printTs :: [T] -> IO ()
        printTs ts = mapM_ (printT [] (tsDepth ts)) ts

    printTs intervals



main :: IO ()
main = do

    args <- getArgs
    mapM_ printElf args
