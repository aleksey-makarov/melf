{-# LANGUAGE DataKinds #-}

module HelloWorld (helloWorld) where

import Prelude as P

import Control.Monad.Catch
import Control.Monad.State
import Data.Word

import AsmAarch64

msg :: String
msg = "Hello World!\n"

sys_exit, sys_write :: Word16
sys_write = 64
sys_exit = 93

helloWorld :: MonadCatch m => StateT CodeState m ()
helloWorld = do

    start <- label
    exportSymbol "_start" start
    mov x0 1
    helloString <- ascii msg
    adr x1 helloString
    mov x2 $ fromIntegral $ P.length msg
    mov x8 sys_write
    svc 0

    mov x0 0
    mov x8 sys_exit
    svc 0
