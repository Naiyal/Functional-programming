amicableNumbersLazy :: Integer -> Integer
amicableNumbersLazy limit = sum $ takeWhile (< limit) (filter isAmicable [1..])




isAmicable :: Integer -> Bool
isAmicable a = let b = sumDivisors a in b /= a && sumDivisors b == a

sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]