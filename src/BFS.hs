import qualified Data.Map as M
import qualified Data.Set as S

bfs :: (Eq a, Ord a, Show a) => M.Map a [a] -> a -> S.Set a -> [[a]] -> [a]
bfs graph destination visited0 (path@(origin : _) : queue)
  | origin == destination = reverse path
  | S.member origin visited0 = bfs graph destination visited0 queue
  | otherwise =
      bfs graph destination visited1 $
        queue ++ maybe [] (map (: path)) (M.lookup origin graph)
  where
    visited1 = S.insert origin visited0
bfs _ _ _ _ = []

main :: IO ()
main = print $ bfs graph 'e' S.empty ["a"]
  where
    graph = M.fromList [('a', "bc"), ('b', "ac"), ('c', "d"), ('d', "ae")]
