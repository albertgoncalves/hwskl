test :: [(String, Bool)] -> IO ()
test =
  ( \(results, labels) -> do
      putStrLn results
      if labels == ""
        then return ()
        else putStrLn labels
  )
    . mconcat
    . map (\(label, bool) -> if bool then (".", "") else ("!", '\n' : label))

main :: IO ()
main =
  test
    [ ("a", True),
      ("b", False),
      ("c", True),
      ("d", False)
    ]
