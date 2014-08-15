toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n = digit : toDigits rest
    where rest = floor ((fromIntegral n) / 10)
          digit = n - rest * 10

