data Tree a
  = Leaf
  | Node Int (Tree a) a (Tree a)
  deriving (Eq, Ord)

pad :: Int -> String
pad n = concat $ replicate n " "

showTree :: (Show a) => Tree a -> Int -> String
showTree Leaf _ = "Leaf"
showTree (Node h Leaf x Leaf) _ =
  concat ["Node ", show h, " Leaf ", show x, " Leaf"]
showTree (Node h l x r) offset =
  concat ["Node ", show h, indent, l', indent, show x, indent, r']
  where
    offset' = offset + 2
    l' = showTree l offset'
    r' = showTree r offset'
    indent = "\n" ++ pad offset'

instance (Show a) => Show (Tree a) where
  show t = pad n ++ showTree t n
    where
      n = 0

height :: Tree a -> Int
height Leaf = -1
height (Node h _ _ _) = h

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h l x' r)
  | lHeight < rHeight = Node h l' x' r
  | lHeight > rHeight = Node h l x' (insert x r)
  | otherwise = Node (height l' + 1) l' x' r
  where
    lHeight = height l
    rHeight = height r
    l' = insert x l

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf

main :: IO ()
main = print $ foldTree "abcdefghij"
