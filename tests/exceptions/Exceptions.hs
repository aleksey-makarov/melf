{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception.ChainedException

import Control.Exception hiding (try)
import Control.Monad.Catch
import Test.Tasty
import Test.Tasty.HUnit

--------------------------------------------

f :: IO ()
f = $chainedError "some error"

f' :: IO ()
f' = $chainedError'

data TestException = TestException deriving Show
instance Exception TestException

fe :: IO ()
fe = throwM $ TestException

f1 :: IO ()
f1 = $addContext "some context" $ f

f1' :: IO ()
f1' = $addContext' $ f

fe' :: IO ()
fe' = $addContext' $ fe

fe'e :: IO ()
fe'e = $addContext "some context 2" $ fe'

--------------------------------------------

checkExceptions :: IO () -> String -> IO ()
checkExceptions m s = try m >>= \ case
    Right _                    -> assertFailure "it should throw an error"
    Left (e :: SomeException)  -> show e @?= s

main :: IO ()
main = defaultMain $ testGroup "exceptions" [ testCase "f"    $ checkExceptions f    "some error (tests/exceptions/Exceptions.hs:17)"
                                            , testCase "f'"   $ checkExceptions f'   "(tests/exceptions/Exceptions.hs:20)"
                                            , testCase "fe"   $ checkExceptions fe   "TestException"
                                            , testCase "f1"   $ checkExceptions f1   "some context (tests/exceptions/Exceptions.hs:29) // some error (tests/exceptions/Exceptions.hs:17)"
                                            , testCase "f1'"  $ checkExceptions f1'  "(tests/exceptions/Exceptions.hs:32) // some error (tests/exceptions/Exceptions.hs:17)"
                                            , testCase "fe'"  $ checkExceptions fe'  "(tests/exceptions/Exceptions.hs:35) // TestException"
                                            , testCase "fe'e" $ checkExceptions fe'e "some context 2 (tests/exceptions/Exceptions.hs:38) // (tests/exceptions/Exceptions.hs:35) // TestException"
                                            ]