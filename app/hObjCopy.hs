{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Binary
import Data.Elf
import System.Environment

objCopy :: String -> String -> IO ()
objCopy inFile outFile = do
    (elf :: Elf) <- decodeFile inFile
    encodeFile outFile elf

main :: IO ()
main = do
    [inFile, outFile] <- getArgs
    objCopy inFile outFile
