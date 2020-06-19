import Control.Monad ((<=<))
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Time
  = Time
      { getHour :: Int,
        getMin :: Int
      }

instance Show Time where
  show (Time h 0) = show h ++ " o'clock"
  show (Time h m@1) = show m ++ " minute past " ++ show h
  show (Time h 15) = "quarter past " ++ show h
  show (Time h 45) = "quarter to " ++ show (h + 1)
  show (Time h 30) = "half past " ++ show h
  show (Time h m)
    | m < 30 = show m ++ " minutes past " ++ show h
    | otherwise = show (60 - m) ++ " minutes to " ++ show (h + 1)

parse :: String -> Maybe Time
parse = f <=< mapM readMaybe . lines
  where
    f [h, m] = Just $ Time h m
    f _ = Nothing

main :: IO ()
main =
  mapM_ print $
    mapMaybe
      parse
      [ "5\n00",
        "5\n01",
        "5\n10",
        "5\n15",
        "5\n28",
        "5\n30",
        "5\n40",
        "5\n45",
        "5\n47"
      ]
