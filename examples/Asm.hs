{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

-- https://github.com/fused-effects/fused-effects -- see Related work

module Asm
    ( CodeState (..)
    , Register (..)
    , PoolRef
    , mov
    , ldr
    , svc
    , pool
    , getCode
    , x0, x1, x2, x8
    , w0, w1
    ) where

import Prelude as P

-- import Control.Lens
-- import Control.Lens.Operators
import Control.Monad.Catch
import Control.Monad.State as MS
import Data.Bits
import Data.ByteString.Lazy as BSL
import Data.ByteString.Builder
import Data.Kind
-- import qualified Data.List as L
-- import Data.Map.Lazy as M
import Data.Word

import Data.Elf.Headers

data CodeState = CodeState
    { codeReversed :: [Word32]
    -- { codeReversed :: [BSL.ByteString]
    -- , _labels    :: M.Map String Word64
    -- , _labelsAll :: M.Map String Word64
    -- , offset       :: Word64
    }

-- makeLenses ''CodeState

data PoolRef = PoolRef

type Register :: ElfClass -> Type
data Register c = R Word

x0, x1, x2, x8 :: Register 'ELFCLASS64
x0 = R 0
x1 = R 1
x2 = R 2
x8 = R 8

w0, w1 :: Register 'ELFCLASS32
w0 = R 0
w1 = R 1

-- splitBits :: (Bits b, Integral b) => b -> [Word8]
-- splitBits b = narrow <$> L.unfoldr f b
--     where
--         narrow x = fromIntegral (x .&. 0xff)
--         f x = Just (x, x `shiftR` 8)
--
-- packFiniteBits :: (FiniteBits b, Integral b) => b -> ByteString
-- packFiniteBits b = pack $ P.take n $ splitBits b
--     where
--         n = (finiteBitSize b + 7) `div` 8

emit :: MonadState CodeState m => Word32 -> m ()
emit i = do
    CodeState {..} <- get
    put CodeState { codeReversed = i : codeReversed
                  -- , offset = offset + (fromIntegral $ BSL.length bs)
                  }

mov :: MonadState CodeState m => Register w -> Word16 -> m ()
mov _ _ = return ()

ldr :: (MonadState CodeState m, MonadThrow m) => Register w -> PoolRef -> m ()
ldr _ _ = return ()

svc :: MonadState CodeState m => Word16 -> m ()
svc imm = emit $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

pool :: (MonadState CodeState m, MonadThrow m) => ByteString -> m PoolRef
pool _ = return PoolRef

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
    CodeState cr <- execStateT n (CodeState [])
    return $ toLazyByteString $ mconcat $ fmap word32LE $ P.reverse cr
