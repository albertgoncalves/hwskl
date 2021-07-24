{-# LANGUAGE FlexibleInstances #-}

{- NOTE: See `Week 5` over at
 - `https://www.cis.upenn.edu/~cis194/spring13/lectures.html`.
 -}

import Control.Applicative hiding (Const)
import Control.Arrow (first)
import Control.Monad (void, (>=>))
import Data.Char (digitToInt, isDigit, isSpace)
import Data.List (foldl')

class Expr a where
  lit :: Int -> a
  add :: a -> a -> a
  mul :: a -> a -> a

{- NOTE Building block of a computation with some state of type `s` threaded
 - through it, possibly resulting in a value of type `r` along with some
 - updated state.
 -}
newtype State s r = State (s -> Maybe (r, s))

data ParseExpr
  = Const Int
  | Add' ParseExpr ParseExpr
  | Mul' ParseExpr ParseExpr
  deriving (Show)

instance Functor (State s) where
  fmap f (State g) = State $ fmap (first f) . g

instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  State f <*> State g = State $ f >=> (\(r, s') -> fmap (first r) . g $ s')

instance Alternative (State s) where
  empty = State $ const Nothing
  State f <|> State g = State $ \s -> f s <|> g s

{- NOTE: A parser threads some `String` state through a computation that
 - produces some value of type `a`.
 -}
type Parser a = State String a

{- NOTE: Parse one numerical digit. -}
digit :: Parser Int
digit = State parseDigit
  where
    parseDigit [] = Nothing
    parseDigit (c : cs)
      | isDigit c = Just (fromIntegral $ digitToInt c, cs)
      | otherwise = Nothing

{- NOTE: Parse an integer. The integer may be prefixed with a negative sign. -}
num :: Parser Int
num =
  maybe id (const negate) <$> optional (char '-') <*> (toInt' <$> some digit)
  where
    toInt' :: [Int] -> Int
    toInt' = foldl' ((+) . (* 10)) 0

{- NOTE: Parse a single white space character. -}
space :: Parser ()
space = State parseSpace
  where
    parseSpace [] = Nothing
    parseSpace (c : cs)
      | isSpace c = Just ((), cs)
      | otherwise = Nothing

{- NOTE: Consume zero or more white space characters. -}
eatSpace :: Parser ()
eatSpace = void (many space)

{- NOTE: Parse a specific character. -}
char :: Char -> Parser Char
char c = State parseChar
  where
    parseChar [] = Nothing
    parseChar (x : xs)
      | x == c = Just (c, xs)
      | otherwise = Nothing

{- NOTE: Parse one of our two supported operator symbols. -}
op :: Parser (ParseExpr -> ParseExpr -> ParseExpr)
op = (Add' <$ char '+') <|> (Mul' <$ char '*')

{- NOTE: Succeed only if the end of the input has been reached. -}
eof :: Parser ()
eof = State parseEof
  where
    parseEof [] = Just ((), [])
    parseEof _ = Nothing

{- NOTE: Parse an infix arithmetic expression consisting of integers, plus
 - signs, multiplication signs, and parentheses.
 -}
parseExpr' :: Parser ParseExpr
parseExpr' =
  eatSpace
    *> ((buildOp <$> nonOp <*> (eatSpace *> op) <*> parseExpr') <|> nonOp)
  where
    buildOp x op' y = x `op'` y
    nonOp = char '(' *> parseExpr' <* char ')' <|> Const <$> num

{- NOTE: Run a parser over a 'String' returning the parsed value and the
 - remaining 'String' data.
 -}
execParser :: Parser a -> String -> Maybe (a, String)
execParser (State f) = f

{- NOTE: Run a parser over a 'String' returning the parsed value. -}
evalParser :: Parser a -> String -> Maybe a
evalParser = (fmap fst .) . execParser

{- NOTE: Parse an arithmetic expression using the supplied semantics for
 - integral constants, addition, and multiplication.
 -}
parseExpr :: (Expr a) => String -> Maybe a
parseExpr = (convert <$>) . evalParser (parseExpr' <* eof)
  where
    convert (Const x) = lit x
    convert (Add' x y) = add (convert x) (convert y)
    convert (Mul' x y) = mul (convert x) (convert y)

data StackVal
  = IVal Int
  | Void
  deriving (Show)

data StackExp
  = PushI Int
  | Add
  | Mul
  deriving (Show)

type Stack = [StackVal]

type Program = [StackExp]

stackVM :: Program -> Either String StackVal
stackVM = execute []

errType :: String -> Either String a
errType op' = Left $ "Encountered '" <> op' <> "' opcode with ill-typed stack."

errUnderflow :: String -> Either String a
errUnderflow op' = Left $ "Stack underflow with '" <> op' <> "' opcode."

execute :: Stack -> Program -> Either String StackVal
execute [] [] = Right Void
execute (s : _) [] = Right s
execute s (PushI x : xs) = execute (IVal x : s) xs
execute (IVal s1 : IVal s2 : ss) (Add : xs) = execute (IVal (s1 + s2) : ss) xs
execute (_ : _ : _) (Add : _) = errType "Add"
execute _ (Add : _) = errUnderflow "Add"
execute (IVal s1 : IVal s2 : ss) (Mul : xs) = execute (IVal (s1 * s2) : ss) xs
execute (_ : _ : _) (Mul : _) = errType "Mul"
execute _ (Mul : _) = errUnderflow "Mul"

instance Expr Program where
  lit x = [PushI x]
  add l r = l <> r <> [Add]
  mul l r = l <> r <> [Mul]

compile :: String -> Either String StackVal
compile s = maybe (Left "Unable to parse input.") stackVM (parseExpr s)

main :: IO ()
main = do
  print $ stackVM $ mul (lit 3) $ add (lit 3) (lit 5)
  print $ compile "a"
  print $ compile "(3 * -4) +"
  print $ compile "(3 * -4) + 5"
