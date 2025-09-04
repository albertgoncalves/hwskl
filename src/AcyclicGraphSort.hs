import qualified Data.Map as M
import qualified Data.Set as S

remove :: (Eq a) => a -> M.Map a (S.Set a) -> M.Map a (S.Set a)
remove = M.map . S.filter . (/=)

prune :: M.Map a (S.Set a) -> M.Map a (S.Set a)
prune = M.filterWithKey $ const $ not . S.null

loop :: (Ord a) => M.Map a (S.Set a) -> S.Set a -> [a] -> [a]
loop _ _ [] = []
loop graph visited (node : nodes)
  | S.member node visited = undefined
  | M.member node graph = loop graph (S.insert node visited) $ nodes ++ [node]
  | otherwise = node : loop (prune $ remove node graph) S.empty nodes

sort :: (Ord a) => M.Map a (S.Set a) -> [a]
sort graph
  | M.null graph = []
  | otherwise = loop (prune graph) S.empty $ S.toList $ M.foldr S.union (M.keysSet graph) graph

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
