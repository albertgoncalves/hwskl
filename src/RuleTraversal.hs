import Control.Exception (assert)
import Data.Map.Lazy (Map, fromList, insert, (!))

-- NOTE: See `https://adventofcode.com/2020/day/19`.

type Key = Int

type Rules = Map Key Rule

data Rule
  = Lit Char
  | Seq [Key]
  | Alt [Key] [Key]
  deriving (Show)

traverse' :: Rules -> Rule -> (String, String) -> [(String, String)]
traverse' _ (Lit x) (cs', c : cs) = [(cs' ++ [c], cs) | c == x]
traverse' _ (Lit _) (_, []) = []
traverse' _ (Seq []) cs = [cs]
traverse' rules (Seq (k : ks)) cs =
  traverse' rules (Seq ks) =<< traverse' rules (rules ! k) cs
traverse' rules (Alt l r) cs =
  traverse' rules (Seq l) cs <> traverse' rules (Seq r) cs

main :: IO ()
main = do
  assert
    ( map
        ( \r ->
            sum $
              map
                (\s -> fromEnum $ elem (s, []) $ traverse' r (r ! 0) ([], s))
                strings
        )
        [rules, foldr (uncurry insert) rules inserts]
        == [3, 12]
    )
    $ putStrLn "All good!"
  where
    inserts :: [(Key, Rule)]
    inserts =
      [ (8, [42] `Alt` [42, 8]),
        (11, [42, 31] `Alt` [42, 11, 31])
      ]
    rules :: Rules
    rules =
      fromList
        [ (14, Lit 'b'),
          (1, Lit 'a'),
          (0, Seq [8, 11]),
          (11, Seq [42, 31]),
          (28, Seq [16, 1]),
          (4, Seq [1, 1]),
          (22, Seq [14, 14]),
          (8, Seq [42]),
          (18, Seq [15, 15]),
          (24, Seq [14, 1]),
          (42, [9, 14] `Alt` [10, 1]),
          (9, [14, 27] `Alt` [1, 26]),
          (10, [23, 14] `Alt` [28, 1]),
          (5, [1, 14] `Alt` [15, 1]),
          (19, [14, 1] `Alt` [14, 14]),
          (12, [24, 14] `Alt` [19, 1]),
          (16, [15, 1] `Alt` [14, 14]),
          (31, [14, 17] `Alt` [1, 13]),
          (6, [14, 14] `Alt` [1, 14]),
          (2, [1, 24] `Alt` [14, 4]),
          (13, [14, 3] `Alt` [1, 12]),
          (15, [1] `Alt` [14]),
          (17, [14, 2] `Alt` [1, 7]),
          (23, [25, 1] `Alt` [22, 14]),
          (20, [14, 14] `Alt` [1, 15]),
          (3, [5, 14] `Alt` [16, 1]),
          (27, [1, 6] `Alt` [14, 18]),
          (21, [14, 1] `Alt` [1, 14]),
          (25, [1, 1] `Alt` [1, 14]),
          (26, [14, 22] `Alt` [1, 20]),
          (7, [14, 5] `Alt` [1, 21])
        ]
    strings :: [String]
    strings =
      [ "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
        "bbabbbbaabaabba",
        "babbbbaabbbbbabbbbbbaabaaabaaa",
        "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
        "bbbbbbbaaaabbbbaaabbabaaa",
        "bbbababbbbaaaaaaaabbababaaababaabab",
        "ababaaaaaabaaab",
        "ababaaaaabbbaba",
        "baabbaaaabbaaaababbaababb",
        "abbbbabbbbaaaababbbbbbaaaababb",
        "aaaaabbaabaaaaababaa",
        "aaaabbaaaabbaaa",
        "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
        "babaaabbbaaabaababbaabababaaab",
        "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
      ]
