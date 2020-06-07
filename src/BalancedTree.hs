data Tree a
  = Leaf
  | Node Int (Tree a) a (Tree a)
  deriving (Eq, Ord)

pad :: Int -> String
pad n = concat $ replicate n " "

showTree :: (Show a) => Int -> Tree a -> String
showTree _ Leaf = "Leaf"
showTree _ (Node h Leaf x Leaf) =
  concat ["(Node ", show h, " Leaf ", show x, " Leaf)"]
showTree offset (Node h l x r) =
  concat ["(Node ", show h, indent, lShow, indent, show x, indent, rShow, ")"]
  where
    offset' = offset + 2
    indent = "\n" ++ pad offset'
    lShow = showTree offset' l
    rShow = showTree offset' r

instance (Show a) => Show (Tree a) where
  show = (pad n ++) . showTree n
    where
      n = 0

height :: Tree a -> Int
height Leaf = -1
height (Node h _ _ _) = h

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h l x' r)
  | lHeight < rHeight = Node h l' x' r
  | lHeight > rHeight = Node h l x' r'
  | otherwise = Node (height l' + 1) l' x' r
  where
    lHeight = height l
    rHeight = height r
    l' = insert x l
    r' = insert x r

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr insert Leaf

main :: IO ()
main = print $ foldTree "abcdefghij"
