normalPdf :: Float -> Float -> Float -> Float
normalPdf mu sigma x
  | sigma <= 0.0 = error "sigma <= 0.0"
  | otherwise =
      (1.0 / sqrt (two_sigma_sq * pi))
        * (exp . negate) (((x - mu) ** 2.0) / two_sigma_sq)
  where
    two_sigma_sq = 2.0 * (sigma ** 2.0)

main :: IO ()
main =
  -- NOTE: $ R
  -- > dnorm(
  -- +   c(0.25, 1.25, 3.25, 0.5),
  -- +   c(0.5,  1.5,  2.5,  0.0),
  -- +   c(1.0,  2.0,  3.0,  1.0)
  -- + )
  -- [1] 0.3866681 0.1979188 0.1288894 0.3520653
  mapM_
    print
    [ normalPdf 0.5 1.0 0.25,
      normalPdf 1.5 2.0 1.25,
      normalPdf 2.5 3.0 3.25,
      normalPdf 0.0 1.0 0.5
    ]
