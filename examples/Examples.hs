module Main (main) where

import Data.ByteString.Lazy as BSL
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Data.Elf
import Data.Elf.PrettyPrint

import Lib
import SysCall

mkElf :: Elf' -> FilePath -> IO ()
mkElf elf path = do
    e <- serializeElf elf
    BSL.writeFile path e

main :: IO ()
main = defaultMain $ testGroup "examples"
    [ testCase "lib" (mkElf lib "examples/lib.o")
    , after AllSucceed "lib" $ testGroup "checkLib"
        [ goldenVsFile "dump"   "examples/lib.o.dump.golden"   "examples/lib.o.dump"   (writeElfDump   "examples/lib.o" "examples/lib.o.dump")
        , goldenVsFile "layout" "examples/lib.o.layout.golden" "examples/lib.o.layout" (writeElfLayout "examples/lib.o" "examples/lib.o.layout")
        ]
    , testCase "syscall" (mkElf syscall "examples/syscall")
    , after AllSucceed "syscall" $ testGroup "checkSysCall"
        [ goldenVsFile "dump"   "examples/syscall.dump.golden"   "examples/syscall.dump"   (writeElfDump   "examples/syscall" "examples/syscall.dump")
        , goldenVsFile "layout" "examples/syscall.layout.golden" "examples/syscall.layout" (writeElfLayout "examples/syscall" "examples/syscall.layout")
        ]
    ]
