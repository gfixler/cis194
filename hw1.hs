toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = digit : toDigits rest
    where rest = floor ((fromIntegral n) / 10)
          digit = n - rest * 10

toDigits' :: Integer -> [Integer]
toDigits' n
    | n > 0     = digit : toDigits' digits
    | otherwise = []
    where (x:xs) = show n
          digit  = read (x:[]) :: Integer
          digits = read xs :: Integer

