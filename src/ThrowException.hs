import Control.Exception (Exception (..), catch, throw)

data MyException = ThisException | ThatException
  deriving (Show)

instance Exception MyException

main :: IO ()
main = do
  (catch :: IO () -> (MyException -> IO ()) -> IO ())
    (print (throw ThisException :: Int))
    (print :: MyException -> IO ())
