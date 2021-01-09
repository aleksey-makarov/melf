{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Elf.TH ( mkDeclarations
                   , BaseWord(..)
                   ) where

import Control.Monad
import Language.Haskell.TH

data BaseWord = BaseWord8 | BaseWord16 | BaseWord32 | BaseWord64

newNamePE :: String -> Q (Q Pat, Q Exp)
newNamePE s = do
    n <- newName s
    return (varP n, varE n)

mkDeclarations :: BaseWord -> String -> String -> String -> [(String, Integer)] -> Q [Dec]
mkDeclarations baseType typeNameString patternPrefixString defaultPatternNameString enums = do

    let typeName = mkName typeNameString
    let patternName s = mkName (patternPrefixString ++ s)
    let defaultPatternName = mkName defaultPatternNameString
    let
        baseTypeT =
            case baseType of
                BaseWord8  -> conT $ mkName "Word8"
                BaseWord16 -> conT $ mkName "Word16"
                BaseWord32 -> conT $ mkName "Word32"
                BaseWord64 -> conT $ mkName "Word64"

    let
        newTypeDef =
            newtypeD
                (cxt [])
                typeName
                []
                Nothing
                (normalC typeName [ bangType (bang noSourceUnpackedness noSourceStrictness) baseTypeT ])
                [ derivClause Nothing [ conT (mkName "Eq")
                                      , conT (mkName "Num")
                                      , conT (mkName "Bits")
                                      , conT (mkName "FiniteBits")
                                      , conT (mkName "Ord")
                                      ]
                ]

    let
        mkShowClause (s, n) =
            clause
                [ conP typeName [litP $ IntegerL n] ]
                (normalB [| patternPrefixString ++ s |])
                []

    let showClauses = map mkShowClause enums

    (nP, nE) <- newNamePE "n"
    let
        defaultShowClause =
            clause
                [ conP typeName [nP] ]
                (normalB [| defaultPatternNameString ++ " " ++ show $(nE) |])
                []

    let showInstanceFunctions = funD (mkName "show") (showClauses ++ [ defaultShowClause ])

    let showInstance = instanceD (cxt []) (appT (conT (mkName "Show")) (conT typeName)) [ showInstanceFunctions ]

    let
        mkBinaryInstance :: Q Type -> Q Pat -> Q Exp -> Q Exp -> Q Dec
        mkBinaryInstance typeT putP putE getE =
            instanceD
                (cxt [])
                (appT (conT (mkName "Binary")) typeT)
                [ binaryInstanceGet, binaryInstancePut ]
            where
                binaryInstancePut =
                    funD
                        (mkName "put")
                        [ clause
                            [putP]
                            (normalB $ putE)
                            []
                        ]
                binaryInstanceGet =
                    funD
                        (mkName "get")
                        [ clause
                            []
                            (normalB getE)
                            []
                        ]

    let
        binaryInstancesXe putLe getLe putBe getBe =
            [ do
                (n3P, n3E) <- newNamePE "n"
                mkBinaryInstance
                    (appT (conT $ mkName "Le") (conT typeName))
                    (conP (mkName "Le") [conP typeName [n3P]])
                    [| $putLe $n3E |]
                    [| $(conE $ mkName "Le") <$> ($(conE typeName) <$> $getLe) |]
            , do
                (n3P, n3E) <- newNamePE "n"
                mkBinaryInstance
                    (appT (conT $ mkName "Be") (conT typeName))
                    (conP (mkName "Be") [conP typeName [n3P]])
                    [| $putBe $n3E |]
                    [| $(conE $ mkName "Be") <$> ($(conE typeName) <$> $getBe) |]
            ]

    let
        binaryInstances =
            case baseType of
                BaseWord8 ->
                    [ do
                        (n3P, n3E) <- newNamePE "n"
                        mkBinaryInstance
                            (conT typeName)
                            (conP typeName [n3P])
                            [| putWord8 $n3E |]
                            [| $(conE typeName) <$> getWord8 |]
                    ]
                BaseWord16 -> binaryInstancesXe [| putWord16le |] [| getWord16le |] [| putWord16be |] [| getWord16be |]
                BaseWord32 -> binaryInstancesXe [| putWord32le |] [| getWord32le |] [| putWord32be |] [| getWord32be |]
                BaseWord64 -> binaryInstancesXe [| putWord64le |] [| getWord64le |] [| putWord64be |] [| getWord64be |]

    let
        mkPatterns (s, n) =
            [ patSynSigD
                (patternName s)
                (conT typeName)
            , patSynD
                (patternName s)
                (prefixPatSyn [])
                implBidir
                (conP typeName [litP $ IntegerL n])
            ]

    let
        defaultPatternSig =
            patSynSigD
                defaultPatternName
                (appT (appT arrowT baseTypeT) (conT typeName))

    localName3 <- newName "n"

    let
        defaultPatternDef =
            patSynD
                defaultPatternName
                (prefixPatSyn [localName3])
                implBidir
                (conP typeName [varP localName3])

    let patterns = (join $ map mkPatterns enums) ++ [ defaultPatternSig, defaultPatternDef ]

    sequence $ newTypeDef : showInstance : patterns ++ binaryInstances
