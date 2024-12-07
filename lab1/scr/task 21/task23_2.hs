generateAmicableNumbers :: Integer -> [Integer]
generateAmicableNumbers limit = filter isAmicable [1..limit-1]

sumAmicableNumbersModular :: Integer -> Integer
sumAmicableNumbersModular limit = foldr (+) 0 (generateAmicableNumbers limit)



isAmicable :: Integer -> Bool
isAmicable a = let b = sumDivisors a in b /= a && sumDivisors b == a

sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]