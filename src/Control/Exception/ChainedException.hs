-- |
-- Module      : Control.Exception.ChainedException
-- Description : Exception that keeps the stack of error locations
-- Copyright   : (c) Aleksey Makarov, 2021
-- License     : BSD 3-Clause License
-- Maintainer  : aleksey.makarov@gmail.com
-- Stability   : experimental
-- Portability : portable
-- 
-- Exception that keeps the stack of error locations.


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Control.Exception.ChainedException
    ( ChainedExceptionNext(..)
    , ChainedException(..)
    , chainedError
    , chainedError'
    , addContext
    , addContext'
    ) where

-- https://stackoverflow.com/questions/13379356/finding-the-line-number-of-a-function-in-haskell

import Control.Exception hiding (try, catch)
import Control.Monad.Catch
import Language.Haskell.TH

-- | Data to organize the stack of locations
data ChainedExceptionNext = Null                         -- ^ Null, when the exception was initiated by `chainedError`
                          | NextChained ChainedException -- ^ Null, when the exception was initiated by an exception other than `ChainedException`
                          | Next SomeException           -- ^ next location in the stack

-- | Exception that keeps track of error locations
data ChainedException = ChainedException
    { err   :: String                    -- ^ description of the error
    , ctxt  :: String                    -- ^ location
    , stack :: ChainedExceptionNext      -- ^ stack of locations
    }

instance Show ChainedException where
    show ChainedException{..} = case stack of
        Null           -> showThis
        NextChained ce -> f ce
        Next e         -> f e

        where
            showThis = (if null err then [] else err) ++ " (" ++ ctxt ++ ")"
            f st = show st ++ " // " ++ showThis

instance Exception ChainedException

withFileLine :: Q Exp -> Q Exp
withFileLine f = let loc = fileLine =<< location in appE f loc

fileLine :: Loc -> Q Exp
fileLine loc = let floc = formatLoc loc in [| $(litE $ stringL floc) |]

formatLoc :: Loc -> String
formatLoc loc =
    let
        file = loc_filename loc
        (line, _) = loc_start loc
    in concat [file, ":", show line]

chainedErrorX :: MonadThrow m => String -> String -> m a
chainedErrorX loc s = throwM $ ChainedException s loc Null

-- | @$chainedError "error description"@ results to a `MonadThrow` monad
-- that throws `ChainedException` with @"error description"@.
chainedError :: Q Exp
chainedError = withFileLine [| chainedErrorX |]

-- | @$chainedError'@ is the same as @$chainedError ""@.
chainedError' :: Q Exp
chainedError' = withFileLine [| \ x -> chainedErrorX x [] |]

addContextX :: MonadCatch m => String -> String -> m a -> m a
addContextX loc s m = m `catch` fc `catch` f
    where
        fc :: MonadThrow m => ChainedException -> m a
        fc e = throwM $ ChainedException s loc $ NextChained e
        f :: MonadThrow m => SomeException -> m a
        f e = throwM $ ChainedException s loc $ Next e

-- | @addContext@
addContext :: Q Exp
addContext = withFileLine [| addContextX |]

-- | @addContext'@
addContext' :: Q Exp
addContext' = withFileLine [| \ x -> addContextX x [] |]
