ackermannPeter :: Int -> Int -> Int
ackermannPeter 0 n = succ n
ackermannPeter m 0 = ackermannPeter (pred m) 1
ackermannPeter m n = ackermannPeter (pred m) $ ackermannPeter m $ pred n

main :: IO ()
main = print $ ackermannPeter 3 10
