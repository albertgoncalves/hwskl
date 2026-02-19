import Data.ByteString.Builder (char7, string7, toLazyByteString, word16BE, word8)
import qualified Data.ByteString.Lazy as L
import System.Environment (getEnv)
import System.FilePath ((</>))
import Prelude hiding (writeFile)

main :: IO ()
main = do
  path <- getEnv "PWD"
  L.writeFile (path </> "out" </> "write_bytes.txt") $
    toLazyByteString $
      string7 "Hello"
        <> word8 0x2C
        <> char7 ' '
        <> foldMap word8 [0x77, 0x6F]
        <> foldMap word16BE [0x726C, 0x6421]
        <> word8 0x0A
