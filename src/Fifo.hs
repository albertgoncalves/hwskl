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

pop :: Fifo a -> (Fifo a, Maybe a)
pop (Fifo [] []) = (Fifo [] [], Nothing)
pop (Fifo input (x : output)) = (Fifo input output, Just x)
pop (Fifo input []) = pop $ Fifo [] $ reverse input

peek :: Fifo a -> (Fifo a, Maybe a)
peek (Fifo [] []) = (Fifo [] [], Nothing)
peek (Fifo input output@(x : _)) = (Fifo input output, Just x)
peek (Fifo input []) = peek $ Fifo [] $ reverse input

main :: IO ()
main = do
  print queue
  print $ pop $ flip push 'd' $ fst $ pop queue
  where
    queue = foldl push empty "abc"
