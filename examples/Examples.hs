module Main (main) where

import Data.ByteString.Lazy as BSL
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Data.Elf
import Data.Elf.PrettyPrint

import Obj
import SysCall

mkElf :: FilePath -> Elf -> IO ()
mkElf path elf = do
    e <- serializeElf elf
    BSL.writeFile path e

testElf :: String -> IO Elf -> [ TestTree ]
testElf elfFileName elf =
    [ testCase elfFileName (elf >>= mkElf f)
    , after AllSucceed ("$1 == \"" ++ elfFileName ++ "\"") $ testGroup ("check " ++ elfFileName)
        [ goldenVsFile "dump"   (d <.> "golden") d (writeElfDump   f d)
        , goldenVsFile "layout" (l <.> "golden") l (writeElfLayout f l)
        ]
    ]
    where
        t = "examples"
        f = t </> elfFileName
        d = t </> elfFileName <.> "dump"
        l = t </> elfFileName <.> "layout"

main :: IO ()
main = defaultMain $ testGroup "examples"
    (  testElf "obj.o"   obj
    ++ testElf "syscall" syscall
    )
