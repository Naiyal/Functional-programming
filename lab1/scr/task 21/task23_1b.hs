amicableNumbersRecursion :: Integer -> Integer
amicableNumbersRecursion limit = sumAmicableHelper 1
  where
    sumAmicableHelper n
      | n >= limit = 0
      | isAmicable n = n + sumAmicableHelper (n + 1)
      | otherwise    = sumAmicableHelper (n + 1)


isAmicable :: Integer -> Bool
isAmicable a = let b = sumDivisors a in b /= a && sumDivisors b == a

sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]