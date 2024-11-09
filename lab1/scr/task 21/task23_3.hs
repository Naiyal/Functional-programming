sumAmicableNumbersMap :: Integer -> Integer
sumAmicableNumbersMap limit = sum $ map (\x -> if isAmicable x then x else 0) [1..limit-1]
