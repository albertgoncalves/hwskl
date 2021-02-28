import Control.Exception (assert)

newtype Weighted a = Weighted (a, Float)
  deriving (Show)

getWeight :: Weighted a -> Float
getWeight (Weighted (_, x)) = x

select :: [Weighted a] -> Float -> Weighted a
select xs w =
  assert (w < last ws) $ fst $ last $ takeWhile ((<= w) . snd) $ zip xs ws
  where
    ws = scanl (\l r -> l + getWeight r) 0 xs

main :: IO ()
main =
  mapM_
    ( print
        . select
          (map Weighted [('a', 0.2), ('b', 0.3), ('c', 0.15), ('d', 0.35)])
    )
    [0.0, 0.10, 0.24, 0.25, 0.35, 0.49, 0.5, 0.6, 0.74, 0.75, 0.85, 0.99]
