sumAmicableNumbersDo :: Integer -> Integer
sumAmicableNumbersDo limit = sum [x | x <- [1..limit-1], isAmicable x]
