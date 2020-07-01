import Data.Char (toUpper)
import Data.List (unfoldr)

data Fifo a
  = Fifo
      { getInput :: [a],
        getOutput :: [a]
      }
  deriving (Show)

instance Monoid (Fifo a) where
  mempty = Fifo [] []

instance Semigroup (Fifo a) where
  (Fifo i1 o1) <> (Fifo i2 o2) = Fifo [] $ o1 <> reverse i1 <> o2 <> reverse i2

instance Functor Fifo where
  fmap f (Fifo i o) = Fifo (fmap f i) (fmap f o)

instance Applicative Fifo where
  pure x = Fifo [] [x]
  (Fifo fI fO) <*> (Fifo i o) = Fifo (fI <*> i) (fO <*> o)

push :: Fifo a -> a -> Fifo a
push (Fifo i o) x = Fifo (x : i) o

pop :: Fifo a -> Maybe (a, Fifo a)
pop (Fifo [] []) = Nothing
pop (Fifo i (x : o)) = Just (x, Fifo i o)
pop (Fifo i []) = pop $ Fifo [] $ reverse i

peek :: Fifo a -> Maybe a
peek (Fifo [] []) = Nothing
peek (Fifo _ (x : _)) = Just x
peek (Fifo i []) = peek $ Fifo [] $ reverse i

main :: IO ()
main = do
  print $ pop queue
  print $ peek queue
  print maybeQueue
  print $ do
    queue' <- maybeQueue
    Just $ snd queue' <> (toUpper <$> snd queue')
  print $ unfoldr pop queue
  where
    queue = foldl push mempty "abcdefg"
    push' = flip push
    maybeQueue = pop queue >>= pop . push' 'i' . push' 'h' . snd
