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

-- | Structure to organize the stack of exceptions with locations
data ChainedExceptionNext = Null                         -- ^ exception was initiated by @`chainedError`@
                          | Next SomeException           -- ^ some context was added to @`SomeException`@ by @`addContext`@
                          | NextChained ChainedException -- ^ some context was added to a @`ChainedException`@ by @`addContext`@

-- | Exception that keeps track of error locations
data ChainedException = ChainedException
    { err   :: String               -- ^ description of the error
    , loc   :: Loc                  -- ^ location
    , stack :: ChainedExceptionNext -- ^ stack of locations
    }

formatLoc :: Loc -> String
formatLoc loc =
    let
        file = loc_filename loc
        (line, _) = loc_start loc
    in concat [file, ":", show line]

instance Show ChainedException where
    show ChainedException{..} = showThis ++ case stack of
        Null           -> ""
        NextChained ce -> " // " ++ show ce
        Next e         -> " // " ++ show e
        where
            showThis = concat [err, if null err then "" else " ", "(", formatLoc loc, ")" ]

instance Exception ChainedException

withLoc :: Q Exp -> Q Exp
withLoc f = appE f (location >>= liftLoc)

liftLoc :: Loc -> Q Exp
liftLoc Loc {..} = [| Loc loc_filename loc_package loc_module loc_start loc_end |]

--------------------------------------------------------

chainedErrorX :: MonadThrow m => Loc -> String -> m a
chainedErrorX loc s = throwM $ ChainedException s loc Null

-- | @$chainedError "error description"@ results in a `MonadThrow` monad
-- that throws `ChainedException` with @"error description"@.
chainedError :: Q Exp
chainedError = withLoc [| chainedErrorX |]

-- | @$chainedError'@ is the same as @$`chainedError` ""@
chainedError' :: Q Exp
chainedError' = withLoc [| \ x -> chainedErrorX x [] |]

addContextX :: MonadCatch m => Loc -> String -> m a -> m a
addContextX loc s m = m `catch` f
    where
        f :: MonadThrow m => SomeException -> m a
        f e = throwM $ ChainedException s loc $ case fromException e of
            Just ce -> NextChained ce
            Nothing -> Next e

-- | @addContext "context description" m@ where @m@ is a `MonadCatch` will run @m@ and 
-- | append a context to any exception thrown from @m@
addContext :: Q Exp
addContext = withLoc [| addContextX |]

-- | @addContext'@ is the same as @addContext ""@
addContext' :: Q Exp
addContext' = withLoc [| \ x -> addContextX x [] |]
