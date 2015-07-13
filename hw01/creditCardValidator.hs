charToDigit :: Char -> Integer
charToDigit n = head [d | (c,d) <- zip ['0'..'9'] [0..9], c == n]

toDigits :: Integer -> [Integer]
toDigits n
    | n > 0     = map charToDigit $ show n
    | otherwise = []

toDigitsRev = reverse . toDigits

doubleEvenIndices :: [Integer] -> [Integer]
doubleEvenIndices []  = []
doubleEvenIndices [x] = [x]
doubleEvenIndices (x:y:ys) = x : 2 * y : doubleEvenIndices ys

doubleEveryOther = reverse . doubleEvenIndices . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `rem` 10 == 0

