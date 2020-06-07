data Tree a
  = Leaf
  | Node Int (Tree a) a (Tree a)
  deriving (Eq, Ord)

duplicate :: Int -> String -> String
duplicate = (>>) . enumFromTo 1

showTree :: (Show a) => Tree a -> Int -> String
showTree Leaf _ = "Leaf"
showTree (Node n Leaf x Leaf) _ =
  concat ["Node ", show n, " Leaf ", show x, " Leaf"]
showTree (Node n l x r) n' =
  concat ["Node ", show n, indent, l', indent, show x, indent, r']
  where
    n'' = n' + 2
    l' = showTree l n''
    r' = showTree r n''
    indent = "\n" ++ duplicate n'' " "

instance (Show a) => Show (Tree a) where
  show t = showTree t 0

height :: Tree a -> Int
height Leaf = -1
height (Node n _ _ _) = n

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n l x' r)
  | lHeight < rHeight = Node n l' x' r
  | rHeight < lHeight = Node n l x' (insert x r)
  | otherwise = Node (height l' + 1) l' x' r
  where
    lHeight = height l
    rHeight = height r
    l' = insert x l

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf

main :: IO ()
main = print $ foldTree "abcdefghij"
