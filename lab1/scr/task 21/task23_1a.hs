amicableNumbersTail :: Integer -> Integer
amicableNumbersTail limit = go 1 0
  where
    go n acc
      | n >= limit = acc
      | isAmicable n = go (n + 1) (acc + n)
      | otherwise    = go (n + 1) acc

isAmicable :: Integer -> Bool
isAmicable a = let b = sumDivisors a in b /= a && sumDivisors b == a

sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]
