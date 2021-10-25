{-# LANGUAGE DataKinds #-}

module HelloWorld (helloWorld) where

import Prelude as P

import Control.Monad.Catch
import Control.Monad.State

import AsmAarch64

msg :: String
msg = "Hello World!\n"

helloWorld :: MonadCatch m => StateT CodeState m ()
helloWorld = do

    label >>= exportSymbol "_start"      -- _start:
    mov x0 1                             --     mov x0, #1
    ascii msg >>= adr x1                 --     ldr x1, =msg
    mov x2 $ fromIntegral $ P.length msg --     ldr x2, =len
    mov x8 64                            --     mov x8, #64 // write()
    svc 0                                --     svc #0
                                         --
    mov x0 0                             --     mov x0, #0
    mov x8 93                            --     mov x8, #93 // exit()
    svc 0                                --     svc #0
                                         --
                                         -- .ascii "Hello World!\n"
