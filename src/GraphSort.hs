import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

expand :: M.Map String (S.Set String) -> S.Set String -> S.Set String
expand graph labels =
  case S.minView labels of
    Nothing -> S.empty
    Just (label, remaining) ->
      let parents = fromMaybe S.empty $ M.lookup label graph
       in S.union parents $
            expand (M.delete label graph) $
              S.union remaining parents

reorder :: [(String, S.Set String)] -> [[String]]
reorder [] = []
reorder (node@(label, parents) : remaining)
  | S.null parents = [label] : reorder (map (S.delete label <$>) remaining)
  | S.member label parents =
      S.toList parents
        : reorder
          ( map ((`S.difference` parents) <$>) $
              filter (not . (`S.member` parents) . fst) remaining
          )
  | otherwise = reorder $ remaining ++ [node]

main :: IO ()
main =
  mapM_ print $
    reorder $
      sortOn snd $
        zip labels $
          map (expand (M.fromList nodes) . S.singleton) labels
  where
    nodes =
      [ ("b", S.singleton "a"),
        ("c", S.singleton "e"),
        ("a", S.empty),
        ("f", S.empty),
        ("d", S.fromList ["a", "c", "f"]),
        ("e", S.singleton "d"),
        ("g", S.fromList ["a", "b"]),
        ("h", S.fromList ["c", "d", "e", "a", "b"])
      ]
    labels = map fst nodes
