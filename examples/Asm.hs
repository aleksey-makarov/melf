{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- https://github.com/fused-effects/fused-effects -- see Related work

module Asm
    ( CodeState (..)
    , RegisterWidth (..)
    , Register (..)
    , label
    , adc
    , getCode
    , x0, x1
    , w0, w1
    ) where

import Control.Monad.State as MS
import Data.ByteString.Lazy as BSL
import Data.Kind

data CodeState = CodeState { code :: BSL.ByteString }

data RegisterWidth = X | W

type Register :: RegisterWidth -> Type
data Register c = R Word

x0, x1 :: Register 'X
x0 = R 0
x1 = R 1

w0, w1 :: Register 'W
w0 = R 0
w1 = R 1

label :: MonadState CodeState m => String -> m ()
label = undefined

adc :: MonadState CodeState m => String -> m ()
adc = undefined

getCode :: Monad m => StateT CodeState m () -> m BSL.ByteString
getCode n = code <$> execStateT n (CodeState empty)
