{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception.ChainedException

f1 :: IO ()
f1 = $chainedError "some error @1"

f2 :: IO ()
f2 = $chainedError'

f11 :: IO ()
f11 = $addContext "some context @11" $ f1

f21 :: IO ()
f21 = $addContext' $ f1

checkExceptions :: IO () -> IO ()
checkExceptions x = x

main :: IO ()
main = defaultMain $ testGroup "exceptions" [ testCase "f1"  $ checkExceptions f1
                                            , testCase "f2"  $ checkExceptions f2
                                            , testCase "f11" $ checkExceptions f11
                                            , testCase "f21" $ checkExceptions f21
                                            ]
