ackermannPeter :: Int -> Int -> Int
ackermannPeter 0 n = n + 1
ackermannPeter m 0 = ackermannPeter (m - 1) 1
ackermannPeter m n = ackermannPeter (m - 1) $ ackermannPeter m (n - 1)

main :: IO ()
main = mapM_ (print . ackermannPeter 3) [0 .. 9]
