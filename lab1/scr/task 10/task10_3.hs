sumPrimesMap :: Integer -> Integer
sumPrimesMap limit = sum $ map (\x -> if isPrime x then x else 0) [2..limit-1]




isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. (floor . sqrt $ fromIntegral n)]