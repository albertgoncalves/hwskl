data Fifo a
  = Fifo
      { getInput :: [a],
        getOutput :: [a]
      }
  deriving (Show)

empty :: Fifo a
empty = Fifo [] []

push :: Fifo a -> a -> Fifo a
push (Fifo input output) x = Fifo (x : input) output

pop :: Fifo a -> Maybe (Fifo a, a)
pop (Fifo [] []) = Nothing
pop (Fifo input (x : output)) = Just (Fifo input output, x)
pop (Fifo input []) = pop $ Fifo [] $ reverse input

peek :: Fifo a -> Maybe (Fifo a, a)
peek (Fifo [] []) = Nothing
peek (Fifo input output@(x : _)) = Just (Fifo input output, x)
peek (Fifo input []) = peek $ Fifo [] $ reverse input

main :: IO ()
main = do
  print queue
  print $ pop queue >>= pop . flip push 'd' . fst
  where
    queue = foldl push empty "abc"
