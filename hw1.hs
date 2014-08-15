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


charToDigit :: Char -> Integer
charToDigit n
    | n == '0' = 0
    | n == '1' = 1
    | n == '2' = 2
    | n == '3' = 3
    | n == '4' = 4
    | n == '5' = 5
    | n == '6' = 6
    | n == '7' = 7
    | n == '8' = 8
    | n == '9' = 9

toDigits'' :: Integer -> [Integer]
toDigits'' n
    | n > 0     = map charToDigit $ show n
    | otherwise = []

