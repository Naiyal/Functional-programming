-- Генерация последовательности чисел
generatePrimes :: Integer -> [Integer]
generatePrimes limit = filter isPrime [2..limit-1]

-- Фильтрация простых чисел и свёртка с суммой
sumPrimesModular :: Integer -> Integer
-- sumPrimesModular limit = foldr (+) 0 (generatePrimes limit)
sumPrimesModular limit = sum (generatePrimes limit)



isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. (floor . sqrt $ fromIntegral n)]