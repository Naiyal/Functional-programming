generateAmicableNumbers :: Integer -> [Integer]
generateAmicableNumbers limit = filter isAmicable [1..limit-1]

sumAmicableNumbersModular :: Integer -> Integer
sumAmicableNumbersModular limit = foldr (+) 0 (generateAmicableNumbers limit)
