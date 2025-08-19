import Control.Monad (void, zipWithM)
import Data.List (uncons)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Test.HUnit (runTestTTAndExit, test, (~?=))
import Text.ParserCombinators.ReadP (ReadP, char, eof, many1, munch1, readP_to_S, skipMany, (<++))

-- NOTE: See `https://reasonablypolymorphic.com/blog/nimic/`.

data Term
  = Ident String
  | Var String
  | List [Term]
  deriving (Eq, Ord)

instance Show Term where
  show (Ident s) = s
  show (Var s) = "#" ++ s
  show (List ts) = "(" ++ unwords (map show ts) ++ ")"

space :: ReadP ()
space = void $ char ' '

token :: ReadP String
token = munch1 (`notElem` " #()")

term :: ReadP Term
term =
  skipMany space
    *> ( (List <$> (char '(' *> many1 term <* char ')'))
           <++ (Var <$> (char '#' *> token))
           <++ (Ident <$> token)
       )

parse :: String -> Term
parse = head . map fst . filter (null . snd) . readP_to_S (term <* skipMany space <* eof)

bind :: Term -> Term -> Maybe (Maybe (Term, Term))
bind (Var _) _ = Nothing
bind t (Var s) = Just $ Just (Var s, t)
bind a b
  | a == b = Just Nothing
bind _ _ = Nothing

replace :: (Ord a) => a -> M.Map a a -> a
replace x = fromMaybe x . M.lookup x

rewrite :: Term -> Term -> Term -> Maybe Term
rewrite (Ident a) (Ident b) c
  | a == b = Just c
rewrite (List as) (List bs) c
  | length as == length bs = do
      m <- M.fromList . catMaybes <$> zipWithM bind as bs
      return $ case c of
        List cs -> List $ map (`replace` m) cs
        _ -> replace c m
rewrite _ _ _ = Nothing

tryRewrites :: Term -> [(Term, Term)] -> Maybe Term
tryRewrites t = (fst <$>) . uncons . mapMaybe (uncurry $ rewrite t)

reduce :: [(Term, Term)] -> Term -> Maybe (Term, [(Term, Term)])
reduce rules t@(Ident _) = (,rules) <$> tryRewrites t rules
reduce rules (List [Ident "macro", a, b]) = Just (Ident "defined", (a, b) : rules)
reduce rules0 t1@(List (t0 : ts)) =
  case reduce rules0 t0 of
    Just (t2, rules1) ->
      let t3 = List $ t2 : ts
       in case tryRewrites t3 rules1 of
            Just t4 -> Just (t4, rules1)
            Nothing -> Just (t3, rules1)
    Nothing ->
      case tryRewrites t1 rules0 of
        Just t2 -> Just (t2, rules0)
        Nothing -> Nothing
reduce _ _ = undefined

tryReduce :: [(Term, Term)] -> Term -> (Term, [(Term, Term)])
tryReduce rules0 t0 =
  case reduce rules0 t0 of
    Just (t1, rules1) -> tryReduce rules1 t1
    Nothing -> (t0, rules0)

main :: IO ()
main =
  runTestTTAndExit $
    test $
      map
        (\(actual, expected) -> fst (tryReduce [] $ parse actual) ~?= parse expected)
        [ ("(hello world)", "(hello world)"),
          ("(macro this that)", "defined"),
          ("(macro defined done)", "done"),
          ("((macro (#a ; #b) #b) ; ((macro (true #a #b) #a) ; (true yes no)))", "yes"),
          ( "((((macro (#a ; #b) #b) ; ((macro (false #a #b) #b) ; (false yes no))) !) ?)",
            "((no !) ?)"
          )
        ]
