import qualified Data.Map as M
import qualified Data.Set as S

remove :: (Eq a) => a -> M.Map a (S.Set a) -> M.Map a (S.Set a)
remove = fmap . S.filter . (/=)

prune :: M.Map a (S.Set a) -> M.Map a (S.Set a)
prune = M.filterWithKey $ const $ not . null

loop :: (Ord a) => M.Map a (S.Set a) -> S.Set a -> [S.Set a]
loop graph nodes
  | null nodes && null graph = []
  | not $ null neighbors =
      neighbors : loop (prune $ foldr remove graph neighbors) (S.difference nodes neighbors)
  | otherwise = undefined
  where
    neighbors = S.difference nodes $ M.keysSet graph

sort :: (Ord a) => M.Map a (S.Set a) -> [S.Set a]
sort graph
  | null graph = []
  | otherwise = loop (prune graph) $ foldr S.union (M.keysSet graph) graph

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
