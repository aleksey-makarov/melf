module Main (main) where

import Data.ByteString.Lazy as BSL
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Data.Elf
import Data.Elf.PrettyPrint

import Lib

mkLib :: FilePath -> IO ()
mkLib path = do
    e <- serializeElf lib
    BSL.writeFile path e

main :: IO ()
main = defaultMain $ testGroup "examples"
    [ testCase "lib" (mkLib "examples/lib.o")
    , after AllSucceed "lib" $ testGroup "checkLib"
        [ goldenVsFile "dump"   "examples/lib.o.dump.golden"   "examples/lib.o.dump"   (writeElfDump   "examples/lib.o" "examples/lib.o.dump")
        , goldenVsFile "layout" "examples/lib.o.layout.golden" "examples/lib.o.layout" (writeElfLayout "examples/lib.o" "examples/lib.o.layout")
        ]
    ]
