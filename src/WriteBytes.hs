import Data.ByteString.Builder (toLazyByteString, word8)
import Data.ByteString.Lazy (writeFile)
import System.Environment (getEnv)
import System.FilePath ((</>))
import Prelude hiding (writeFile)

main :: IO ()
main = do
  path <- getEnv "WD"
  writeFile (path </> "out" </> "write_bytes.txt") $
    toLazyByteString $
      foldMap
        word8
        [ 0x48,
          0x65,
          0x6C,
          0x6C,
          0x6F,
          0x2C,
          0x20,
          0x77,
          0x6F,
          0x72,
          0x6C,
          0x64,
          0x21
        ]
