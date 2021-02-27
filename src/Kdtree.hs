import Data.List (sortBy)
import Data.Semigroup (Sum)

class Point a where
  dims :: a -> Int
  coord :: Int -> a -> Float

  next :: Int -> a -> Int
  next dim = mod (dim + 1) . dims

  diffOn :: Int -> a -> a -> Float
  diffOn dim l r = coord dim l - coord dim r

  cmpOn :: Int -> a -> a -> Ordering
  cmpOn dim l r = compare (coord dim l) (coord dim r)

  dist :: a -> a -> Float
  dist l r =
    sqrt $ sum $ map (\dim -> diffOn dim l r ^ (2 :: Int)) [0 .. dims l - 1]

data Tree a
  = Node Int a (Tree a) (Tree a)
  | Leaf
  deriving (Show)

makeTree :: Point a => [a] -> Tree a
makeTree = f 0
  where
    f _ [] = Leaf
    f dim xs@(x : _) = Node dim x'' (f dim' l) (f dim' r)
      where
        dim' = next dim x
        (l, x'', r) =
          case splitAt (length xs `div` 2) (sortBy (cmpOn dim) xs) of
            (l', x' : r') -> (l', x', r')
            ([x'], []) -> ([], x', [])
            _ -> undefined

withinRadius :: Point a => Tree a -> a -> Float -> (Sum Int, [a])
withinRadius Leaf _ _ = (0, [])
withinRadius (Node dim split l r) point radius =
  case cmpOn dim point split of
    EQ -> x <> l' <> r'
    LT -> let xl = x <> l' in if overlap then xl <> r' else xl
    GT -> let xr = x <> r' in if overlap then xr <> l' else xr
  where
    overlap = abs (diffOn dim point split) < radius
    l' = withinRadius l point radius
    r' = withinRadius r point radius
    x = (1, [split | dist point split <= radius])

data Vec2 = Vec2 Float Float
  deriving (Show)

instance Point Vec2 where
  dims _ = 2
  coord 0 (Vec2 x _) = x
  coord 1 (Vec2 _ y) = y
  coord _ _ = undefined

main :: IO ()
main =
  mapM_
    (print . uncurry (withinRadius tree))
    [ (Vec2 0.0 0.0, 0.5),
      (Vec2 3.0 3.0, 1.0),
      (Vec2 3.0 4.0, 1.0),
      (Vec2 4.5 5.0, 1.5),
      (Vec2 6.0 5.0, 2.5),
      (Vec2 3.0 10.0, 5.0)
    ]
  where
    tree =
      makeTree $
        map
          (uncurry Vec2)
          [ (0.0, 4.0),
            (0.0, 6.0),
            (0.0, 8.0),
            (0.0, 9.0),
            (1.0, 1.0),
            (1.0, 4.0),
            (1.0, 5.0),
            (2.0, 2.0),
            (2.0, 5.0),
            (2.0, 7.0),
            (3.0, 4.0),
            (4.0, 1.0),
            (4.0, 3.0),
            (4.0, 4.0),
            (5.0, 2.0),
            (5.0, 4.0),
            (5.0, 5.0),
            (8.0, 3.0),
            (9.0, 2.0)
          ]
