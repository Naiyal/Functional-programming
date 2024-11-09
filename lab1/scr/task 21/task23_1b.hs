amicableNumbersRecursion :: Integer -> Integer
amicableNumbersRecursion limit = sumAmicableHelper 1
  where
    sumAmicableHelper n
      | n >= limit = 0
      | isAmicable n = n + sumAmicableHelper (n + 1)
      | otherwise    = sumAmicableHelper (n + 1)
