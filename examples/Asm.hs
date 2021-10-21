{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Asm
    ( CodeState
    , Register
    , RelativeRef
    , adr
    , mov
    , ldr
    , svc
    , ascii
    , label
    , exportSymbol
    , assemble
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
import Data.Int
import Data.Kind
import Data.Word

import Data.Elf
import Data.Elf.Constants
import Data.Elf.Headers

newtype CodeOffset  = CodeOffset  { getCodeOffset  :: Int64 }  deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)
newtype Instruction = Instruction { getInstruction :: Word32 } deriving (Eq, Show, Ord, Num, Enum, Real, Integral, Bits, FiniteBits)

data RelativeRef = CodeRef !CodeOffset
                 | PoolRef !CodeOffset

-- Args:
-- Offset of the instruction
-- Offset of the pool
type InstructionGen = CodeOffset -> CodeOffset -> Either String Instruction

data CodeState = CodeState
    { offsetInPool    :: CodeOffset
    , poolReversed    :: [Builder]
    , codeReversed    :: [InstructionGen]
    , symbolsRefersed :: [(String, RelativeRef)]
    }

emit' :: MonadState CodeState m => InstructionGen -> m ()
emit' g = modify f where
    f CodeState {..} = CodeState { codeReversed = g : codeReversed
                                 , ..
                                 }

emit :: MonadState CodeState m => Instruction -> m ()
emit i = emit' $ \ _ _ -> Right i

emitPool :: MonadState CodeState m => Word -> ByteString -> m RelativeRef
emitPool a bs = state f where
    f CodeState {..} =
        let
            offsetInPool' = align a offsetInPool
            o = builderRepeatZero $ fromIntegral $ offsetInPool' - offsetInPool
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

isPower2 :: (Bits i, Num i) => i -> Bool
isPower2 n = n .&. (n - 1) == 0

align :: Word -> CodeOffset -> CodeOffset
align a _ | not (isPower2 a) = error "align is not a power of 2"
align 0 n = n
align a n = (n + a' - 1) .&. complement (a' - 1)
    where a' = fromIntegral a

builderRepeatZero :: Int -> Builder
builderRepeatZero n = mconcat $ P.take n $ P.repeat $ word8 0

class IsElfClass w => AArch64Instr w where
    b64 :: Register w -> Word32

instance AArch64Instr 'ELFCLASS32 where
    b64 _ = 0

instance AArch64Instr 'ELFCLASS64 where
    b64 _ = 1

-- | C6.2.10 ADR
adr :: (MonadState CodeState m, AArch64Instr w) => Register w -> RelativeRef -> m ()
adr (R n) rr =  emit' f
    where

        offsetToImm :: CodeOffset -> Either String Word32
        offsetToImm (CodeOffset o) =
            if not $ isBitN 19 o
                    then Left "offset is too big"
                    else
                        let
                            immlo = o .&. 3
                            immhi = (o `shiftR` 2)  .&. 0x7ffff
                        in
                            Right $ fromIntegral $ (immhi `shift` 5) .|. (immlo `shift` 29)

        f :: InstructionGen
        f instrAddr poolOffset = do
            imm <- offsetToImm $ findOffset poolOffset rr - instrAddr
            return $ Instruction $  0x10000000
                                .|. imm
                                .|. n

-- | C6.2.187 MOV (wide immediate)
mov :: (MonadState CodeState m, AArch64Instr w) => Register w -> Word16 -> m ()
mov r@(R n) imm = emit $ Instruction $ (b64 r `shift` 31)
                                    .|. 0x52800000
                                    .|. (fromIntegral imm `shift` 5)
                                    .|. n

-- | The number can be represented with bitN bits
isBitN ::(Num b, Bits b, Ord b) => Int -> b -> Bool
isBitN bitN w =
    let
        m = complement $ (1 `shift` bitN) - 1
        h = w .&. m
    in if w >= 0 then h == 0 else h == m

findOffset :: CodeOffset -> RelativeRef -> CodeOffset
findOffset _poolOffset (CodeRef codeOffset)   = codeOffset
findOffset  poolOffset (PoolRef offsetInPool) = poolOffset + offsetInPool

-- | C6.2.132 LDR (literal)
ldr :: (MonadState CodeState m, AArch64Instr w) => Register w -> RelativeRef -> m ()
ldr r@(R n) rr = emit' f
    where

        offsetToImm19 :: CodeOffset -> Either String Word32
        offsetToImm19 (CodeOffset o) =
            if o .&. 0x3 /= 0
                then Left "offset is not aligned"
                else if not $ isBitN 19 o
                    then Left "offset is too big"
                    else Right $ fromIntegral $ o `shiftR` 2

        f :: InstructionGen
        f instrAddr poolOffset = do
            imm19 <- offsetToImm19 $ findOffset poolOffset rr - instrAddr
            return $ Instruction $ (b64 r `shift` 30)
                                .|. 0x18000000
                                .|. (imm19 `shift` 5)
                                .|. n

-- | C6.2.317 SVC
svc :: MonadState CodeState m => Word16 -> m ()
svc imm = emit $ 0xd4000001 .|. (fromIntegral imm `shift` 5)

ascii :: (MonadState CodeState m, MonadThrow m) => String -> m RelativeRef
ascii s = emitPool 1 $ BSLC.pack s

exportSymbol :: MonadState CodeState m => String -> RelativeRef -> m ()
exportSymbol s r = modify f where
    f (CodeState {..}) = CodeState { symbolsRefersed = (s, r) : symbolsRefersed
                                   , ..
                                   }

instructionSize :: Num b => b
instructionSize = 4

-- FIXME: move to Chained, create macros
returnEither :: MonadThrow m => Either String a -> m a
returnEither (Left s)  = $chainedError s
returnEither (Right a) = return a

zeroIndexStringItem :: ElfSymbolXX 'ELFCLASS64
zeroIndexStringItem = ElfSymbolXX "" 0 0 0 0 0

assemble :: MonadCatch m => ElfSectionIndex -> StateT CodeState m () -> m (BSL.ByteString, [ElfSymbolXX 'ELFCLASS64])
assemble textSecN m = do

    CodeState {..} <- execStateT m (CodeState 0 [] [] [])

    -- resolve txt

    let
        poolOffset = instructionSize * (fromIntegral $ P.length codeReversed)
        poolOffsetAligned = align 8 poolOffset

        f :: (InstructionGen, CodeOffset) -> Either String Instruction
        f (ff, n) = ff n poolOffsetAligned

    code <- returnEither $ mapM f $ P.zip (P.reverse codeReversed) (fmap (instructionSize *) [CodeOffset 0 .. ])

    let
        codeBuilder = mconcat $ fmap (word32LE . getInstruction) code
        txt = toLazyByteString $ codeBuilder
                              <> (builderRepeatZero $ fromIntegral $ poolOffsetAligned - poolOffset)
                              <> (mconcat $ P.reverse poolReversed)

    -- resolve symbolTable

    let
        ff :: (String, RelativeRef) -> ElfSymbolXX 'ELFCLASS64
        ff (s, r) =
            let
                steName  = s
                steBind  = STB_Global
                steType  = STT_NoType
                steShNdx = textSecN
                steValue = fromIntegral $ findOffset poolOffset r
                steSize  = 0
            in
                ElfSymbolXX{..}

        symbolTable = fmap ff $ P.reverse symbolsRefersed

    return $ (txt, zeroIndexStringItem : symbolTable)
