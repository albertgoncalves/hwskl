import Data.List (intercalate)
import qualified Data.Set as S
import Text.Printf (printf)

-- NOTE: See `https://pnwamk.github.io/sst-tutorial/`.

data Type
  = TypeVar String
  | TypeAnd Type Type
  | TypeOr Type Type
  | TypeTuple [Type]
  deriving (Eq, Ord)

instance Show Type where
  show (TypeVar var) = var
  show (TypeAnd a b) = printf "(%s & %s)" (show a) (show b)
  show (TypeOr a b) = printf "(%s | %s)" (show a) (show b)
  show (TypeTuple ts) = printf "(%s)" $ intercalate ", " $ map show ts

unpack :: Type -> S.Set Type
unpack t@(TypeVar _) = S.singleton t
unpack (TypeAnd a0 b0) =
  case (pack a1, pack b1) of
    (TypeTuple as, TypeTuple bs)
      | length as == length bs ->
          S.singleton $
            TypeTuple $
              zipWith (\a -> pack . unpack . TypeAnd a) as bs
    _ -> S.intersection a1 b1
  where
    a1 = unpack a0
    b1 = unpack b0
unpack (TypeOr a0 b0) =
  case (pack a1, pack b1) of
    (TypeTuple as, TypeTuple bs)
      | length as == length bs ->
          S.singleton $
            TypeTuple $
              zipWith (\a -> pack . unpack . TypeOr a) as bs
    _ -> S.union a1 b1
  where
    a1 = unpack a0
    b1 = unpack b0
unpack (TypeTuple ts) =
  S.fromList $ map TypeTuple $ explode $ map (pack . unpack) ts

pack :: S.Set Type -> Type
pack types =
  case S.minView types of
    Just (t, ts) | S.null ts -> t
    Just (t, ts) -> TypeOr t $ pack ts
    Nothing -> undefined

explode :: [Type] -> [[Type]]
explode [] = [[]]
explode (TypeOr a b : ts0) = map (a :) ts1 ++ map (b :) ts1
  where
    ts1 = explode ts0
explode (t : ts) = map (t :) $ explode ts

main :: IO ()
main = do
  print types
  print $ S.toList $ unpack types
  where
    types =
      TypeAnd
        (TypeTuple [TypeOr (TypeVar "a") (TypeVar "b"), TypeVar "c"])
        $ TypeOr
          ( TypeOr
              (TypeTuple [TypeVar "a", TypeOr (TypeVar "b") (TypeVar "c")])
              (TypeVar "d")
          )
        $ TypeTuple
          [TypeVar "b", TypeVar "c"]
