import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Util
import System.Environment

import Data.Elf
import Data.Elf.PrettyPrint

-- FIXME: use instance Binary Elf'
printFile :: String -> IO ()
printFile fileName = do
    bs <- fromStrict <$> BS.readFile fileName
    elf <- parseElf bs
    doc <- printElf elf
    putDocW 80 (doc  <> line)

main :: IO ()
main = do
    args <- getArgs
    mapM_ printFile args
