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
    exportSymbol "_start" start          -- _start:
    mov x0 1                             --     mov x0, #1
    helloString <- ascii msg             --
    adr x1 helloString                   --     ldr x1, =msg
    mov x2 $ fromIntegral $ P.length msg --     ldr x2, =len
    mov x8 sys_write                     --     mov x8, #64 // write()
    svc 0                                --     svc #0
                                         --
    mov x0 0                             --     mov x0, #0
    mov x8 sys_exit                      --     mov x8, #93 // exit()
    svc 0                                --     svc #0
                                         --
                                         -- .ascii "Hello World!\n"
