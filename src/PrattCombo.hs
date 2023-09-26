import Control.Monad.State (State, modify, runState, state)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Printf (printf)

data Expr
  = ExprInt Int
  | ExprVar String
  | ExprCall [Expr]

instance Show Expr where
  show (ExprInt int) = show int
  show (ExprVar var) = var
  show (ExprCall [expr]) = show expr
  show (ExprCall exprs) = printf "(%s)" (unwords $ map show exprs)

data Parser = Parser
  { parserRanks :: M.Map String (Int, Int),
    parserTokens :: [String]
  }
  deriving (Show)

setRank :: String -> Int -> Int -> State Parser ()
setRank op leftRank rightRank = modify $ \p ->
  p {parserRanks = M.insert op (leftRank, rightRank) $ parserRanks p}

getRank :: String -> State Parser (Maybe (Int, Int))
getRank token = state $ \p -> (M.lookup token $ parserRanks p, p)

popToken :: State Parser (Maybe String)
popToken = state $ \p ->
  case parserTokens p of
    (token : tokens) -> (Just token, p {parserTokens = tokens})
    [] -> (Nothing, p)

peekToken :: State Parser (Maybe String)
peekToken = state $ \p ->
  case parserTokens p of
    (token : _) -> (Just token, p)
    [] -> (Nothing, p)

assertToken :: (String -> Bool) -> State Parser ()
assertToken f = do
  maybeToken <- popToken
  case maybeToken of
    Just token | f token -> return ()
    _ -> undefined

unfold :: (Monad m) => m (Maybe a) -> m [a]
unfold x = maybe (return []) ((<$> unfold x) . (:)) =<< x

isInt :: String -> Bool
isInt [] = False
isInt "-" = False
isInt ('-' : cs) = all isDigit cs
isInt cs = all isDigit cs

isIdent :: String -> Bool
isIdent (c : cs) = isAlpha c && all isAlphaNum cs
isIdent [] = False

parseAtom :: State Parser (Maybe Expr)
parseAtom = do
  maybeToken <- peekToken
  case maybeToken of
    Just token | isInt token -> do
      _ <- popToken
      return $ Just $ ExprInt $ read token
    Just token | isIdent token -> do
      _ <- popToken
      return $ Just $ ExprVar token
    Just "(" -> do
      _ <- popToken
      expr <- parseExpr
      assertToken (== ")")
      return $ Just expr
    _ -> return Nothing

parseCall :: State Parser Expr
parseCall = do
  call <- unfold parseAtom
  case call of
    [] -> undefined
    _ -> return $ ExprCall call

-- NOTE: See `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`.
parseExprRight :: Int -> Expr -> State Parser (Int, Expr)
parseExprRight rank0 left = do
  maybeOp <- peekToken
  case maybeOp of
    Just op -> do
      maybeRank <- getRank op
      case maybeRank of
        Just (leftRank1, rightRank1) | rank0 <= leftRank1 -> do
          _ <- popToken
          right <- parseExprLeft rightRank1
          parseExprRight rank0 (ExprCall [ExprVar op, left, right])
        _ -> return (rank0, left)
    _ -> return (rank0, left)

parseExprLeft :: Int -> State Parser Expr
parseExprLeft rank = snd <$> (parseExprRight rank =<< parseCall)

parseExpr :: State Parser Expr
parseExpr = parseExprLeft 0

parse :: State Parser Expr
parse = do
  maybeToken <- peekToken
  case maybeToken of
    Just "infixl" -> do
      _ <- popToken
      op <- fromJust <$> popToken
      rank <- (* 2) . read . fromJust <$> popToken
      setRank op (pred rank) rank
      parse
    Just "infixr" -> do
      _ <- popToken
      op <- fromJust <$> popToken
      rank <- (* 2) . read . fromJust <$> popToken
      setRank op rank $ pred rank
      parse
    _ -> parseExpr

tokenize :: String -> [String]
tokenize [] = []
tokenize (' ' : cs) = tokenize cs
tokenize ('(' : cs) = "(" : tokenize cs
tokenize (')' : cs) = ")" : tokenize cs
tokenize cs = a : tokenize b
  where
    (a, b) = break (`elem` " ()") cs

main :: IO ()
main =
  print $
    runState parse $
      Parser M.empty $
        tokenize $
          unwords
            [ "infixl + 1",
              "infixl - 1",
              "infixl * 2",
              "infixr . 3",
              "f0 (f1 a b0) 0 * f2 c + f3 - 1 * f . g . h"
            ]
