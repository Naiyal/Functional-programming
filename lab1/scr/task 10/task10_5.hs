-- Генерация бесконечного списка простых чисел
primes :: [Integer]
primes = filter isPrime [2..]

sumPrimesLazy :: Integer -> Integer
sumPrimesLazy limit = sum $ takeWhile (< limit) primes



isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. (floor . sqrt $ fromIntegral n)]