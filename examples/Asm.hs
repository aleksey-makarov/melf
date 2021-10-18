{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE RankNTypes #-}

module Asm
    ( CodeState
    , Register
    , PoolOffset
    , mov
    , ldr
    , svc
    , ascii
    , getCode
    , x0, x1, x2, x8
    , w0, w1
    ) where

import Prelude as P

import Control.Exception.ChainedException
import Control.Monad.Catch
import Control.Monad.State as MS
import Data.Bits
import Data.ByteString.Builder
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as BSLC
import Data.Kind
import Data.Word

import Data.Elf.Headers


-- Args:
-- Offset of the instruction
-- Offset of the pool
type InstructionGen = Word32 -> Word32 -> Either String Word32

data CodeState = CodeState
    { offsetInPool :: Word32
    , poolReversed :: [Builder]
    , codeReversed :: [InstructionGen]
    }

type Register :: ElfClass -> Type
data Register c = R Word32

type PoolOffset = Word32

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

emit' :: MonadState CodeState m => InstructionGen -> m ()
emit' f = do
    CodeState {..} <- get
    put CodeState { codeReversed = f : codeReversed
                  , ..
                  }

emit :: MonadState CodeState m => Word32 -> m ()
emit i = emit' $ \ _ _ -> Right i

isPower2 :: (Bits i, Integral i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0

align :: (Num n, Integral n, Eq n, Bits n) => n -> n -> n
align a _ | not (isPower2 a) = error "align is not a power of 2"
align 0 n = n
align a n = (n + a - 1) .&. complement (a - 1)

builderRepeatZero :: Integral n => n -> Builder
builderRepeatZero n = mconcat $ P.take (fromIntegral n) $ P.repeat $ word8 0

emitPool :: MonadState CodeState m => Word32 -> ByteString -> m Word32
emitPool a bs = do
    CodeState {..} <- get
    let
        offsetInPool' = align a offsetInPool
        o = builderRepeatZero $ offsetInPool' - offsetInPool
    put CodeState { offsetInPool = (fromIntegral $ BSL.length bs) + offsetInPool'
                  , poolReversed = lazyByteString bs : o : poolReversed
                  , ..
                  }
    return offsetInPool'

class IsElfClass w => AArch64Instr w where
    b64 :: Register w -> Word32

instance AArch64Instr 'ELFCLASS32 where
    b64 _ = 0

instance AArch64Instr 'ELFCLASS64 where
    b64 _ = 1

mov :: (MonadState CodeState m, AArch64Instr w) => Register w -> Word16 -> m ()
mov r@(R n) imm = emit $  (b64 r `shift` 31)
                      .|. 0x52800000
                      .|. (fromIntegral imm `shift` 5)
                      .|. n

ldr :: (MonadState CodeState m, AArch64Instr w) => Register w -> PoolOffset -> m ()
ldr r@(R n) poolOffset = emit' f
    where
        f instrAddr offsetInPool =
            let
                imm19 = poolOffset + offsetInPool - instrAddr
            in
                if imm19 >= (1 `shift` 19)
                    then Left "offset is too big"
                    else Right $  (b64 r `shift` 30)
                              .|. 0x18000000
                              .|. (fromIntegral imm19 `shift` 5)
                              .|. n

svc :: MonadState CodeState m => Word16 -> m ()
svc imm = emit $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

ascii :: (MonadState CodeState m, MonadThrow m) => String -> m PoolOffset
ascii s = emitPool 1 $ BSLC.pack s

instructionSize :: Num b => b
instructionSize = 4

-- FIXME: move to Chained, create macros
returnEither :: MonadThrow m => Either String a -> m a
returnEither (Left s)  = $chainedError s
returnEither (Right a) = return a

resolvePool :: MonadCatch m => CodeState -> m BSL.ByteString
resolvePool CodeState {..} = do
    let
        poolOffset = instructionSize * (fromIntegral $ P.length codeReversed)
        poolOffsetAligned = align 8 poolOffset

        f :: (InstructionGen, Word32) -> Either String Word32
        f (ff, n) = ff n poolOffsetAligned

    code <- returnEither $ mapM f $ P.zip (P.reverse codeReversed) (fmap (instructionSize *) [0 .. ])

    let
        codeBuilder = mconcat $ fmap word32LE code

    return $ toLazyByteString $ codeBuilder
                             <> (builderRepeatZero $ poolOffsetAligned - poolOffset)
                             <> (mconcat $ P.reverse poolReversed)

getCode :: MonadCatch m => StateT CodeState m () -> m BSL.ByteString
getCode n = execStateT n (CodeState 0 [] []) >>= resolvePool
