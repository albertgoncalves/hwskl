ackermannPeter :: (Int -> a) -> Int -> Int -> a
ackermannPeter k 0 n = k $ succ n
ackermannPeter k m 0 = ackermannPeter k (pred m) 1
ackermannPeter k m n = ackermannPeter (ackermannPeter k $ pred m) m $ pred n

main :: IO ()
main = print $ ackermannPeter id 3 10
