{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}

module ForwardLabel (forwardLabel) where

import Prelude as P

import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.State

import AsmAarch64

ok :: String
ok = "ok\n"

bad :: String
bad = "bad\n"

forwardLabel :: (MonadCatch m, MonadFix m) => StateT CodeState m ()
forwardLabel = mdo

    label >>= exportSymbol "_start"

    lOk <- ascii ok
    lBad <- ascii bad

    mov x0 1

    adr x1 lOk
    mov x2 $ fromIntegral $ P.length ok

    b skipBad

    adr x1 lBad
    mov x2 $ fromIntegral $ P.length bad

    skipBad <- label

    mov x8 64
    svc 0

    mov x0 0
    mov x8 93
    svc 0
