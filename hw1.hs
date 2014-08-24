charToDigit :: Char -> Integer
charToDigit n = head [d | (c,d) <- zip ['0'..'9'] [0..9], c == n]

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0     = map charToDigit $ show n
    | otherwise = []

toDigitsRev = reverse . toDigits

doubleOddIndices :: [Integer] -> [Integer]
doubleOddIndices []  = []
doubleOddIndices [x] = [2 * x]
doubleOddIndices (x:y:ys) = 2 * x : y : doubleOddIndices ys

doubleEveryOther = reverse . doubleOddIndices . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

