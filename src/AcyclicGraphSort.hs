import qualified Data.Map as M
import qualified Data.Set as S

remove :: (Eq a) => a -> M.Map a (S.Set a) -> M.Map a (S.Set a)
remove x = M.map (S.filter (/= x))

prune :: M.Map a (S.Set a) -> M.Map a (S.Set a)
prune = M.filterWithKey $ \_ -> not . S.null

loop :: (Ord a) => [a] -> M.Map a (S.Set a) -> S.Set a -> [a]
loop [] _ _ = []
loop (node : nodes) graph visited
  | M.member node graph = loop (nodes ++ [node]) graph (S.insert node visited)
  | otherwise = node : loop nodes (prune $ remove node graph) S.empty

sort :: (Ord a) => M.Map a (S.Set a) -> [a]
sort graph
  | M.null graph = []
  | otherwise = loop (S.toList $ M.foldr S.union (M.keysSet graph) graph) (prune graph) S.empty

main :: IO ()
main =
  print $
    sort $
      M.fromList
        [ ('b', S.singleton 'a'),
          ('a', S.empty),
          ('f', S.empty),
          ('d', S.fromList ['a', 'c', 'f']),
          ('e', S.singleton 'd'),
          ('g', S.fromList ['a', 'b']),
          ('h', S.fromList ['c', 'd', 'e', 'a', 'b'])
        ]
