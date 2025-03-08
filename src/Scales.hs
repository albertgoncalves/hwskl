data Pitch
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | Flat Pitch
  | Sharp Pitch
  deriving (Eq)

data Scale
  = Major
  | Minor
  | HarmonicMajor
  | HarmonicMinor
  | MelodicMinor
  deriving (Show)

instance Show Pitch where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"
  show E = "E"
  show F = "F"
  show G = "G"
  show pitch
    | d < 0 = n ++ replicate (-d) 'b'
    | 0 < d = n ++ replicate d '#'
    | otherwise = n
    where
      d = tally pitch
      n = show $ natural pitch

tally :: Pitch -> Int
tally (Flat pitch) = pred $ tally pitch
tally (Sharp pitch) = succ $ tally pitch
tally _ = 0

type Chord = (Scale, [(Int, Pitch -> Pitch)])

allPitches :: [Pitch]
allPitches = cycle [C, D, E, F, G, A, B]

toInt :: Pitch -> Int
toInt A = 9
toInt B = 11
toInt C = 0
toInt D = 2
toInt E = 4
toInt F = 5
toInt G = 7
toInt (Flat pitch) = pred $ toInt pitch
toInt (Sharp pitch) = succ $ toInt pitch

toPitches :: Scale -> [Pitch]
toPitches Major = [C, D, E, F, G, A, B]
toPitches Minor = [C, D, Flat E, F, G, Flat A, Flat B]
toPitches HarmonicMajor = [C, D, E, F, G, Flat A, B]
toPitches HarmonicMinor = [C, D, Flat E, F, G, Flat A, B]
toPitches MelodicMinor = [C, D, Flat E, F, G, A, B]

toInts :: Pitch -> Scale -> [Int]
toInts pitch = map ((+ toInt pitch) . toInt) . toPitches

natural :: Pitch -> Pitch
natural (Flat pitch) = natural pitch
natural (Sharp pitch) = natural pitch
natural pitch = pitch

adjust :: Pitch -> Int -> Pitch
adjust pitch n0
  | (p `mod` 12) == (n0 `mod` 12) = pitch
  | abs (n1 - p) < d = adjust pitch n1
  | abs (n2 - p) < d = adjust pitch n2
  | n0 < p = adjust (Flat pitch) n0
  | p < n0 = adjust (Sharp pitch) n0
  | otherwise = undefined
  where
    p = toInt pitch
    d = abs (p - n0)
    n1 = n0 - 12
    n2 = n0 + 12

makeScale :: Pitch -> Scale -> [Pitch]
makeScale pitch =
  zipWith adjust (take 7 $ dropWhile (/= natural pitch) allPitches)
    . toInts pitch

makeChord :: Pitch -> Chord -> [Pitch]
makeChord pitch = uncurry $ loop . zip [1 ..] . cycle . makeScale pitch
  where
    loop :: [(Int, Pitch)] -> [(Int, Pitch -> Pitch)] -> [Pitch]
    loop _ [] = []
    loop ((i, p) : ps) fs1@((j, f) : fs0)
      | j < i = undefined
      | i == j = f p : loop ps fs0
      | otherwise = loop ps fs1
    loop [] _ = undefined

add :: Int -> (Pitch -> Pitch) -> Chord -> Chord
add degree accidental chord =
  fmap (++ [(degree, accidental)]) $ check $ snd chord
  where
    check :: [(Int, Pitch -> Pitch)] -> Chord
    check [] = chord
    check ((i, _) : fs)
      | degree == i = undefined
      | otherwise = check fs

alter :: Int -> (Pitch -> Pitch) -> Chord -> Chord
alter degree accidental chord = fmap loop $ check $ snd chord
  where
    loop :: [(Int, Pitch -> Pitch)] -> [(Int, Pitch -> Pitch)]
    loop [] = []
    loop (f1@(i, f0) : fs)
      | degree == i = (i, accidental . f0) : fs
      | otherwise = f1 : loop fs

    check :: [(Int, Pitch -> Pitch)] -> Chord
    check [] = undefined
    check ((i, _) : fs)
      | degree == i = chord
      | otherwise = check fs

interval :: Pitch -> Int -> Int -> Pitch
interval pitch steps =
  adjust
    ( last $
        take steps $
          dropWhile (/= natural pitch) $
            cycle $
              toPitches Major
    )
    . (+ toInt pitch)

enharmonic :: Pitch -> [Pitch]
enharmonic pitch = [interval pitch 7 0, interval pitch 2 0]

addDiatonic :: Int -> Chord -> Chord
addDiatonic degree = add degree id

flat3 :: Chord -> Chord
flat3 = alter 3 Flat

flat5 :: Chord -> Chord
flat5 = alter 5 Flat

sharp5 :: Chord -> Chord
sharp5 = alter 5 Sharp

add6 :: Chord -> Chord
add6 = addDiatonic 6

add7 :: Chord -> Chord
add7 = addDiatonic 7

flat7 :: Chord -> Chord
flat7 = alter 7 Flat

addFlat7 :: Chord -> Chord
addFlat7 = flat7 . add7

major :: Chord
major = (Major, [(1, id), (3, id), (5, id)])

minor :: Chord
minor = flat3 major

aug :: Chord
aug = sharp5 major

dim :: Chord
dim = flat5 minor

maj6 :: Chord
maj6 = add6 major

min6 :: Chord
min6 = add6 minor

maj7 :: Chord
maj7 = add7 major

min7 :: Chord
min7 = addFlat7 minor

dom7 :: Chord
dom7 = addFlat7 major

dim7 :: Chord
dim7 = flat7 min7Flat5

min7Flat5 :: Chord
min7Flat5 = flat5 min7

dom7Sharp5 :: Chord
dom7Sharp5 = sharp5 dom7

contains :: (Eq a) => [a] -> [a] -> Bool
contains xs = all (`elem` xs)

search :: Pitch -> Chord -> [(Pitch, Scale)]
search root chord =
  map fst $
    filter snd $
      [ ( (pitch, scale),
          (`contains` (map ((`mod` 12) . toInt) $ makeChord root chord)) $
            map (`mod` 12) $
              toInts pitch scale
        )
        | pitch <-
            [C, Sharp C, D, Sharp D, E, F, Sharp F, G, Sharp G, A, Sharp A, B],
          scale <- [Major, Minor, HarmonicMajor, HarmonicMinor, MelodicMinor]
      ]

main :: IO ()
main = putStrLn $ unlines $ map show $ search C dom7
