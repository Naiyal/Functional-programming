sumPrimesDo :: Integer -> Integer
sumPrimesDo limit = sum [x | x <- [2..limit-1], isPrime x]



isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. (floor . sqrt $ fromIntegral n)]