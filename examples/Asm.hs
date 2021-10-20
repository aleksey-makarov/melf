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
    , RelativeRef
    , mov
    , ldr
    , svc
    , ascii
    , label
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

data RelativeRef = CodeRef Word32
                 | PoolRef Word32

-- Args:
-- Offset of the instruction
-- Offset of the pool
type InstructionGen = Word32 -> Word32 -> Either String Word32

data CodeState = CodeState
    { offsetInPool :: Word32
    , poolReversed :: [Builder]
    , codeReversed :: [InstructionGen]
    }

emit' :: MonadState CodeState m => InstructionGen -> m ()
emit' g = modify f where
    f CodeState {..} = CodeState { codeReversed = g : codeReversed
                                 , ..
                                 }

emit :: MonadState CodeState m => Word32 -> m ()
emit i = emit' $ \ _ _ -> Right i

emitPool :: MonadState CodeState m => Word32 -> ByteString -> m RelativeRef
emitPool a bs = state f where
    f CodeState {..} =
        let
            offsetInPool' = align a offsetInPool
            o = builderRepeatZero $ offsetInPool' - offsetInPool
        in
            ( PoolRef offsetInPool'
            , CodeState { offsetInPool = (fromIntegral $ BSL.length bs) + offsetInPool'
                        , poolReversed = lazyByteString bs : o : poolReversed
                        , ..
                        }
            )

label :: MonadState CodeState m => m RelativeRef
label = CodeRef <$> gets offsetInPool

type Register :: ElfClass -> Type
data Register c = R Word32

x0, x1, x2, x8 :: Register 'ELFCLASS64
x0 = R 0
x1 = R 1
x2 = R 2
x8 = R 8

w0, w1 :: Register 'ELFCLASS32
w0 = R 0
w1 = R 1

isPower2 :: (Bits i, Integral i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0

align :: (Num n, Integral n, Eq n, Bits n) => n -> n -> n
align a _ | not (isPower2 a) = error "align is not a power of 2"
align 0 n = n
align a n = (n + a - 1) .&. complement (a - 1)

builderRepeatZero :: Integral n => n -> Builder
builderRepeatZero n = mconcat $ P.take (fromIntegral n) $ P.repeat $ word8 0

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

isBitN ::(Num b, Bits b) => Int -> b -> Bool
isBitN bitN w = if h == 0 || h == m then True else False
    where
        m = complement $ (1 `shift` bitN) - 1
        h = w .&. m

ldr :: (MonadState CodeState m, AArch64Instr w) => Register w -> RelativeRef -> m ()
ldr r@(R n) rr = emit' f
    where
        f instrAddr poolOffset =
            let
                offset = case rr of
                    CodeRef codeOffset   -> codeOffset - instrAddr
                    PoolRef offsetInPool -> poolOffset + offsetInPool - instrAddr
            in
                if offset .&. 0x3 /= 0
                    then Left "offset is not aligned"
                    else if not $ isBitN 19 offset
                        then Left "offset is too big"
                        else Right $  (b64 r `shift` 30)
                                  .|. 0x18000000
                                  .|. (offset `shift` 3)
                                  .|. n

svc :: MonadState CodeState m => Word16 -> m ()
svc imm = emit $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

ascii :: (MonadState CodeState m, MonadThrow m) => String -> m RelativeRef
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
