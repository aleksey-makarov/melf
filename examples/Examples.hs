module Main (main) where

import Control.Monad.Fix
import Control.Monad.Catch
import Data.Bits
import Data.ByteString.Lazy as BSL
import System.FilePath
import System.Posix.Files
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Data.Elf
import Data.Elf.PrettyPrint

import MkObj
import MkExe
import HelloWorld
import ForwardLabel

makeFileExecutable :: String -> IO ()
makeFileExecutable path = do
    m <- fileMode <$> getFileStatus path
    setFileMode path $ m .|. ownerExecuteMode

helloWorldExe :: MonadCatch m => m Elf
helloWorldExe = do
    (txt, _) <- helloWorld 1
    mkExe txt

forwardLabelExe :: (MonadCatch m, MonadFix m) => m Elf
forwardLabelExe = do
    (txt, _) <- forwardLabel 1
    mkExe txt

helloWorldObj :: MonadCatch m => m Elf
helloWorldObj = mkObj helloWorld

fixTargetName :: String -> String
fixTargetName s = fmap f s
    where
        f '.' = '_'
        f x   = x

writeElf :: FilePath -> Elf -> IO ()
writeElf path elf = do
    e <- serializeElf elf
    BSL.writeFile path e
    makeFileExecutable path

testElf :: String -> IO Elf -> [ TestTree ]
testElf elfFileName elf =
    [ testCase makeTargetName (elf >>= writeElf f)
    , after AllSucceed makeTargetName $ testGroup checkTargetName
        [ goldenVsFile "dump"   (d <.> "golden") d (writeElfDump   f d)
        , goldenVsFile "layout" (l <.> "golden") l (writeElfLayout f l)
        ]
    ]
    where
        makeTargetName  = "make_"  ++ fixTargetName elfFileName
        checkTargetName = "check_" ++ fixTargetName elfFileName
        t = "examples"
        f = t </> elfFileName
        d = t </> elfFileName <.> "dump"
        l = t </> elfFileName <.> "layout"

main :: IO ()
main = defaultMain $ testGroup "examples"
    (  testElf "helloWorldObj.o" helloWorldObj
    ++ testElf "helloWorldExe"   helloWorldExe
    ++ testElf "forwardLabelExe" forwardLabelExe
    )
