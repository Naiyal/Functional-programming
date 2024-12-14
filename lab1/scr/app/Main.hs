import Test.HUnit

sumPrimesTail :: Integer -> Integer
sumPrimesTail limit = go 2 0
  where
    go n acc
      | n >= limit = acc
      | isPrime n  = go (n + 1) (acc + n)
      | otherwise  = go (n + 1) acc

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. (floor . sqrt $ fromIntegral n)]

sumPrimesRecursion :: Integer -> Integer
sumPrimesRecursion limit = sumPrimesHelper 2
  where
    sumPrimesHelper n
      | n >= limit = 0
      | isPrime n  = n + sumPrimesHelper (n + 1)
      | otherwise  = sumPrimesHelper (n + 1)

-- Генерация последовательности чисел
generatePrimes :: Integer -> [Integer]
generatePrimes limit = filter isPrime [2..limit-1]

-- Фильтрация простых чисел и свёртка с суммой
sumPrimesModular :: Integer -> Integer
sumPrimesModular limit = foldr (+) 0 (generatePrimes limit)


sumPrimesMap :: Integer -> Integer
sumPrimesMap limit = sum $ map (\x -> if isPrime x then x else 0) [2..limit-1]


sumPrimesDo :: Integer -> Integer
sumPrimesDo limit = sum [x | x <- [2..limit-1], isPrime x]


-- Генерация бесконечного списка простых чисел
primes :: [Integer]
primes = filter isPrime [2..]

sumPrimesLazy :: Integer -> Integer
sumPrimesLazy limit = sum $ takeWhile (< limit) primes

------------------------------

amicableNumbersTail :: Integer -> Integer
amicableNumbersTail limit = go 1 0
  where
    go n acc
      | n >= limit = acc
      | isAmicable n = go (n + 1) (acc + n)
      | otherwise    = go (n + 1) acc

isAmicable :: Integer -> Bool
isAmicable a = let b = sumDivisors a in b /= a && sumDivisors b == a

sumDivisors :: Integer -> Integer
sumDivisors n = sum [x | x <- [1 .. n `div` 2], n `mod` x == 0]



amicableNumbersRecursion :: Integer -> Integer
amicableNumbersRecursion limit = sumAmicableHelper 1
  where
    sumAmicableHelper n
      | n >= limit = 0
      | isAmicable n = n + sumAmicableHelper (n + 1)
      | otherwise    = sumAmicableHelper (n + 1)

generateAmicableNumbers :: Integer -> [Integer]
generateAmicableNumbers limit = filter isAmicable [1..limit-1]

sumAmicableNumbersModular :: Integer -> Integer
sumAmicableNumbersModular limit = foldr (+) 0 (generateAmicableNumbers limit)


sumAmicableNumbersMap :: Integer -> Integer
sumAmicableNumbersMap limit = sum $ map (\x -> if isAmicable x then x else 0) [1..limit-1]



sumAmicableNumbersDo :: Integer -> Integer
sumAmicableNumbersDo limit = sum [x | x <- [1..limit-1], isAmicable x]


amicableNumbersLazy :: Integer -> Integer
amicableNumbersLazy limit = sum $ takeWhile (< limit) (filter isAmicable [1..])
------------------------

testSumPrimes :: Test
testSumPrimes = TestList [
    TestCase (assertEqual "sumPrimesTail 10" (sumPrimesTail 10) 17),
    TestCase (assertEqual "sumPrimesRecursion 10" (sumPrimesRecursion 10) 17),
    TestCase (assertEqual "sumPrimesModular 10" (sumPrimesModular 10) 17),
    TestCase (assertEqual "sumPrimesMap 10" (sumPrimesMap 10) 17),
    TestCase (assertEqual "sumPrimesDo 10" (sumPrimesDo 10) 17),
    TestCase (assertEqual "sumPrimesLazy 10" (sumPrimesLazy 10) 17),
    TestCase (assertEqual "sumPrimesTail 2000000" (sumPrimesTail 2000000) 142913828922)
    ]

testAmicableNumbers :: Test
testAmicableNumbers = TestList [
    TestCase (assertEqual "amicableNumbersTail 10000" (amicableNumbersTail 10000) 31626),
    TestCase (assertEqual "amicableNumbersRecursion 10000" (amicableNumbersRecursion 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersModular 10000" (sumAmicableNumbersModular 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersMap 10000" (sumAmicableNumbersMap 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersDo 10000" (sumAmicableNumbersDo 10000) 31626),
    TestCase (assertEqual "amicableNumbersLazy 10000" (amicableNumbersLazy 10000) 31626)
    ]

runTests :: IO Counts
runTests = runTestTT $ TestList [testSumPrimes,testAmicableNumbers]
