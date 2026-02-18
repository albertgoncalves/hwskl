-- NOTE: See `https://github.com/taolson/Admiran/blob/main/doc/Lazy.md`.

data Frame
  = Open Int Int
  | Spare Int
  | Strike
  | Extra Int

computeFrame :: Int -> [Frame] -> ([Int], Int, Int)
computeFrame _ [] = ([], 0, 0)
computeFrame x (Open a b : fs) = (x' : xs, a, b)
  where
    x' = x + a + b
    (xs, _, _) = computeFrame x' fs
computeFrame x (Spare a : fs) = (x' : xs, a, 10 - a)
  where
    x' = x + 10 + b
    (xs, b, _) = computeFrame x' fs
computeFrame x (Strike : fs) = (x' : xs, 10, a)
  where
    x' = x + 10 + a + b
    (xs, a, b) = computeFrame x' fs
computeFrame x (Extra a : fs) = (xs, a, b)
  where
    x' = x + a
    (xs, b, _) = computeFrame x' fs

main :: IO ()
main =
  mapM_
    (print . (\(xs, _, _) -> xs) . computeFrame 0)
    [ [ Open 1 4,
        Open 4 5,
        Spare 6,
        Spare 5,
        Strike,
        Open 0 1,
        Spare 7,
        Spare 6,
        Strike,
        Spare 2,
        Extra 6
      ],
      replicate 10 Strike ++ replicate 2 (Extra 10)
    ]
