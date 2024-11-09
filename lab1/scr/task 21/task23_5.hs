amicableNumbersLazy :: Integer -> Integer
amicableNumbersLazy limit = sum $ takeWhile (< limit) (filter isAmicable [1..])
