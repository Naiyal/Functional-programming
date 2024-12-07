sumAmicableNumbersDo :: Integer -> Integer
sumAmicableNumbersDo limit = sum [x | x <- [1..limit-1], isAmicable x]




isAmicable :: Integer -> Bool
isAmicable a = let b = sumDivisors a in b /= a && sumDivisors b == a

sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]