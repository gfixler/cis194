charToDigit :: Char -> Integer
charToDigit n = head [d | (c,d) <- zip ['0'..'9'] [0..9], c == n]

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0     = map charToDigit $ show n
    | otherwise = []

toDigitsRev = reverse . toDigits

doubleOddDigits :: [Integer] -> [Integer]
doubleOddDigits []  = []
doubleOddDigits [x] = [2 * x]
doubleOddDigits (x:y:ys) = 2 * x : y : doubleOddDigits ys

doubleEveryOther = reverse . doubleOddDigits . reverse

