sumAmicableNumbersMap :: Integer -> Integer
sumAmicableNumbersMap limit = sum $ map (\x -> if isAmicable x then x else 0) [1..limit-1]



isAmicable :: Integer -> Bool
isAmicable a = let b = sumDivisors a in b /= a && sumDivisors b == a

sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]