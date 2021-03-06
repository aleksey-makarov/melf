{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Prelude as P

import Control.Lens
-- import Control.Lens.Operators
import Control.Monad.Catch
import Control.Monad.State as MS
import Data.Bits
import Data.ByteString.Lazy as BSL
import Data.Kind
import qualified Data.List as L
import Data.Map.Lazy as M
import Data.Word

data AsmException = AsmException String

instance Show AsmException where
    show (AsmException s) = s

instance Exception AsmException

data CodeState = CodeState
    { _code      :: BSL.ByteString
    , _labels    :: M.Map String Word64
    -- , _labelsAll :: M.Map String Word64
    , _offset    :: Word64
    }

makeLenses ''CodeState

data RegisterWidth = X | W

type Register :: RegisterWidth -> Type
data Register c = R Word

x0, x1 :: Register 'X
x0 = R 0
x1 = R 1

w0, w1 :: Register 'W
w0 = R 0
w1 = R 1

splitBits :: (Bits b, Integral b) => b -> [Word8]
splitBits b = narrow <$> L.unfoldr f b
    where
        narrow x = fromIntegral (x .&. 0xff)
        f x = Just (x, x `shiftR` 8)

packFiniteBits :: (FiniteBits b, Integral b) => b -> ByteString
packFiniteBits b = pack $ P.take n $ splitBits b
    where
        n = (finiteBitSize b + 7) `div` 8

fromMaybeToMonadThrow :: MonadThrow m => String -> Maybe a -> m a
fromMaybeToMonadThrow str mb = maybe (throwM $ AsmException str) return mb

getLabel :: (MonadState CodeState m, MonadThrow m) => String -> m Word64
getLabel str = do
    mAddr <- uses labels (M.lookup str)
    fromMaybeToMonadThrow ("the label \'" ++ str ++ "\' is not defined") mAddr

label :: MonadState CodeState m => String -> m ()
label s = do
    o <- use offset
    labels %= (M.insert s o)

adc :: (MonadState CodeState m, MonadThrow m) => String -> m ()
adc str = do
    l <- getLabel str
    code %= (flip append) (packFiniteBits l)
    offset += 8
    return ()

-- codeStateInitial :: CodeState
-- codeStateInitial = CodeState BSL.empty M.empty 0

-- { _code      :: BSL.ByteString
-- , _labels    :: M.Map String Word64
-- , _labelsAll :: M.Map String Word64
-- , _offset    :: Word64
-- }

-- https://wiki.haskell.org/Tying_the_Knot
-- -- https://www.reddit.com/r/haskell/comments/gxqeo/tying_the_knot_a_really_mind_bending_haskell/
-- http://www.lfcs.inf.ed.ac.uk/reports/97/ECS-LFCS-97-375/
-- https://blog.melding-monads.com/2009/12/30/fun-with-the-lazy-state-monad/
getCode :: MonadCatch m => StateT CodeState m () -> m BSL.ByteString
getCode n = do
    CodeState c _l _o <- execStateT n (CodeState BSL.empty M.empty 0)
    return c
