{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Elf.Exception
    ( ElfException
    , elfError
    , elfError'
    , addContext
    , addContext'
    ) where

-- https://stackoverflow.com/questions/13379356/finding-the-line-number-of-a-function-in-haskell

import Control.Exception hiding (try, catch)
import Control.Monad.Catch
import Language.Haskell.TH

data ElfException = ElfException
    { err   :: String
    , ctxt  :: String
    , stack :: Maybe SomeException
    }

instance Show ElfException where
    show ElfException{..} = maybe showThis f stack
        where
            showThis = (if null err then [] else err ++ " ") ++ showCtxt
            showCtxt = "(" ++ ctxt ++ ")"
            f st = show st ++ " // " ++ showThis

instance Exception ElfException

-- FIXME: add function name
-- FIXME: add context only to ElfExceptions
-- FIXME: add envelope for other types of exceptions which should be of the same type (IOException etc)
-- FIXME: move to a separate module

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

elfErrorX :: MonadThrow m => String -> String -> m a
elfErrorX loc s = throwM $ ElfException s loc Nothing

elfError' :: Q Exp
elfError' = withFileLine [| \ x -> elfErrorX x [] |]

elfError :: Q Exp
elfError = withFileLine [| elfErrorX |]

addContextX :: MonadCatch m => String -> String -> m a -> m a
addContextX loc s m = m `catch` f
    where
        f e = throwM $ ElfException s loc $ Just e

addContext :: Q Exp
addContext = withFileLine [| addContextX |]

addContext' :: Q Exp
addContext' = withFileLine [| \ x -> addContextX x [] |]
