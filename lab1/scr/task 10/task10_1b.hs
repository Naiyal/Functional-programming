sumPrimesRecursion :: Integer -> Integer
sumPrimesRecursion limit = sumPrimesHelper 2
  where
    sumPrimesHelper n
      | n >= limit = 0
      | isPrime n  = n + sumPrimesHelper (n + 1)
      | otherwise  = sumPrimesHelper (n + 1)



isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. (floor . sqrt $ fromIntegral n)]