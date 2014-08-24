charToDigit :: Char -> Integer
charToDigit n = head [d | (c,d) <- zip ['0'..'9'] [0..9], c == n]

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0     = map charToDigit $ show n
    | otherwise = []

