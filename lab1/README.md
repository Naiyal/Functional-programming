# Лабораторная работа №1

**Студент:** Наял М.фахд 
**ИСУ:** 336192  
**Группа:** P3310
**Университет:** НИУ ИТМО  
**Факультет:** Программная инженерия и компьютерная техника  
**Курс:** 3-й курс



**Вариант задач 10/21:**
1. Найти сумму всех простых чисел ниже двух миллионов.
2. Найти сумму всех дружественных чисел ниже 10000.


---


## Описание проблемы

#### В этом проекте представлено несколько решений для задач с сайта Project Euler. Решения реализованы на языке Haskell. 

### Задача 1
 Найти сумму всех простых чисел меньше двух миллионов.

### Задача 2
 Найти сумму всех дружественных чисел меньше 10000. Пара чисел (a, b) считается дружественной, если сумма делителей одного равна другому, и наоборот, при этом  a != b .

---

## Ключевые элементы реализации

### Задача 1 : Сумма простых чисел ниже двух миллионов

**1.Монолитная реализация**

- **С использованием хвостовой рекурсии:**

```haskell
sumPrimesTail :: Integer -> Integer
sumPrimesTail limit = go 2 0
  where
    go n acc
      | n >= limit = acc
      | isPrime n  = go (n + 1) (acc + n)
      | otherwise  = go (n + 1) acc

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2 .. (floor . sqrt $ fromIntegral n)]

```

- **С использованием обычной рекурсии:**

```haskell
sumPrimesRecursion :: Integer -> Integer
sumPrimesRecursion limit = sumPrimesHelper 2
  where
    sumPrimesHelper n
      | n >= limit = 0
      | isPrime n  = n + sumPrimesHelper (n + 1)
      | otherwise  = sumPrimesHelper (n + 1)

```
**2.Модулярное решение (разделение на генерацию, фильтрацию, свёртку)**


```haskell
-- Генерация последовательности чисел
generatePrimes :: Integer -> [Integer]
generatePrimes limit = filter isPrime [2..limit-1]

-- Фильтрация простых чисел и свёртка с суммой
sumPrimesModular :: Integer -> Integer
sumPrimesModular limit = foldr (+) 0 (generatePrimes limit)

```


**3. Генерация последовательности с использованием map**


```haskell
sumPrimesMap :: Integer -> Integer
sumPrimesMap limit = sum $ map (\x -> if isPrime x then x else 0) [2..limit-1]

```
**4. Специальный синтаксис для циклов**


```haskell
sumPrimesDo :: Integer -> Integer
sumPrimesDo limit = sum [x | x <- [2..limit-1], isPrime x]

```

**5. Работа с бесконечными списками (ленивые коллекции)**


```haskell
-- Генерация бесконечного списка простых чисел
primes :: [Integer]
primes = filter isPrime [2..]

sumPrimesLazy :: Integer -> Integer
sumPrimesLazy limit = sum $ takeWhile (< limit) primes


```

**6. Сравнение с кодом Python**


```Python
import math

def is_prime(n):
    """Проверка, является ли число простым."""
    if n <= 1:
        return False
    if n <= 3:
        return True
    if n % 2 == 0 or n % 3 == 0:
        return False
    for i in range(5, int(math.sqrt(n)) + 1, 6):
        if n % i == 0 or n % (i + 2) == 0:
            return False
    return True

def sum_primes(limit):
    """Рекурсивная функция для суммы простых чисел."""
    return sum(n for n in range(2, limit) if is_prime(n))

# Использование
result = sum_primes(2000000)
print(result)


```

**Тестирвоание**


```haskell
-- Тесты для задачи 1: Сумма простых чисел
-- Property to verify that all numbers being summed are prime numbers
prop_sumPrimesArePrimes :: Integer -> Bool
prop_sumPrimesArePrimes limit = all isPrime (generatePrimes limit)

-- Property to verify that the sum of prime numbers gives the same result 
-- regardless of which method (tail recursion, recursion, modular, map, do-notation, or lazy) is used
prop_sumPrimesEquivalence :: Integer -> Bool
prop_sumPrimesEquivalence limit = 
    sumPrimesTail limit == sumPrimesRecursion limit &&
    sumPrimesTail limit == sumPrimesModular limit &&
    sumPrimesTail limit == sumPrimesMap limit &&
    sumPrimesTail limit == sumPrimesDo limit &&
    sumPrimesTail limit == sumPrimesLazy limit

-- Property to ensure that the sum of primes is less than half the product 
-- of the limit and the limit divided by two
prop_sumPrimesLessThanLimit :: Integer -> Bool
prop_sumPrimesLessThanLimit limit = sumPrimesTail limit < limit * (limit `div` 2)

-- Property to ensure the sum of primes is correct for small numbers
-- It compares the result of the tail recursive sum with the sum of manually filtered primes
prop_sumPrimesSmall :: Integer -> Property
prop_sumPrimesSmall limit = limit >= 2 ==> sumPrimesTail limit == sum (filter isPrime [2..limit-1])

-- Test to check if the sum of primes for the number 10 is correct for multiple methods
testSumPrimes10 :: Bool
testSumPrimes10 = sumPrimesTail 10 == 17 && sumPrimesRecursion 10 == 17 && sumPrimesModular 10 == 17

main :: IO ()
main = do
    quickCheck prop_sumPrimesArePrimes
    quickCheck prop_sumPrimesEquivalence
    quickCheck prop_sumPrimesLessThanLimit
    quickCheck prop_sumPrimesSmall
    

*Main>quickCheck prop_sumPrimesArePrimes
+++ OK, passed 100 tests.
*Main>quickCheck prop_sumPrimesEquivalence
+++ OK, passed 100 tests.
*Main>quickCheck prop_sumPrimesLessThanLimit
+++ OK, passed 100 tests.
*Main>quickCheck prop_sumPrimesSmall
+++ OK, passed 100 tests.

```


---

### Задача 2 : Сумма дружественных чисел до 10000

**1.Монолитная реализация**

- **С использованием хвостовой рекурсии:**

```haskell
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


```

- **С использованием обычной рекурсии:**

```haskell
amicableNumbersRecursion :: Integer -> Integer
amicableNumbersRecursion limit = sumAmicableHelper 1
  where
    sumAmicableHelper n
      | n >= limit = 0
      | isAmicable n = n + sumAmicableHelper (n + 1)
      | otherwise    = sumAmicableHelper (n + 1)


```
**2.Модулярное решение (разделение на генерацию, фильтрацию, свёртку)**


```haskell
generateAmicableNumbers :: Integer -> [Integer]
generateAmicableNumbers limit = filter isAmicable [1..limit-1]

sumAmicableNumbersModular :: Integer -> Integer
sumAmicableNumbersModular limit = foldr (+) 0 (generateAmicableNumbers limit)

```


**3. Генерация последовательности с использованием map**


```haskell
sumAmicableNumbersMap :: Integer -> Integer
sumAmicableNumbersMap limit = sum $ map (\x -> if isAmicable x then x else 0) [1..limit-1]


```
**4. Специальный синтаксис для циклов**

```haskell
sumAmicableNumbersDo :: Integer -> Integer
sumAmicableNumbersDo limit = sum [x | x <- [1..limit-1], isAmicable x]


```

**5. Работа с бесконечными списками (ленивые коллекции)**


```haskell
amicableNumbersLazy :: Integer -> Integer
amicableNumbersLazy limit = sum $ takeWhile (< limit) (filter isAmicable [1..])


```

**6. Сравнение с кодом Python**


```Python
def sum_of_divisors(n):
    """Вычисление суммы делителей числа."""
    return sum(x for x in range(1, n // 2 + 1) if n % x == 0)

def sum_amicable(limit):
    """Рекурсивная функция для суммы дружественных чисел."""
    total = 0
    for a in range(2, limit):
        b = sum_of_divisors(a)
        if a != b and sum_of_divisors(b) == a:
            total += a
    return total

# Использование
result = sum_amicable(10000)
print(result)



```

**Тестирвоание**


```haskell
-- Тесты для задачи 2: Сумма дружественных чисел
-- Property to verify that all numbers being summed are amicable numbers
prop_sumAmicableNumbersAreAmicable :: Integer -> Bool
prop_sumAmicableNumbersAreAmicable limit = all isAmicable (generateAmicableNumbers limit)

-- Property to verify that all methods give the same result for the sum of amicable numbers
prop_sumAmicableEquivalence :: Integer -> Bool
prop_sumAmicableEquivalence limit =
    amicableNumbersTail limit == amicableNumbersRecursion limit &&
    amicableNumbersTail limit == sumAmicableNumbersModular limit &&
    amicableNumbersTail limit == sumAmicableNumbersMap limit &&
    amicableNumbersTail limit == sumAmicableNumbersDo limit &&
    amicableNumbersTail limit == amicableNumbersLazy limit

-- Property to ensure that the sum of amicable numbers is less than the given limit
prop_sumAmicableLessThanLimit :: Integer -> Bool
prop_sumAmicableLessThanLimit limit = limit > 0 ==> sumAmicableNumbersDo limit < limit * (limit `div` 2)

-- Test to verify the sum of amicable numbers for a fixed value (10)
testAmicableNumbers10 :: Bool
testAmicableNumbers10 = amicableNumbersTail 10 == 0 &&
                        amicableNumbersRecursion 10 == 0 &&
                        sumAmicableNumbersModular 10 == 0 &&
                        sumAmicableNumbersMap 10 == 0 &&
                        sumAmicableNumbersDo 10 == 0 &&
                        amicableNumbersLazy 10 == 0

main :: IO ()
main = do
    quickCheck prop_sumAmicableNumbersAreAmicable
    quickCheck prop_sumAmicableEquivalence
    quickCheck prop_sumAmicableLessThanLimit
    quickCheck testAmicableNumbers10


+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
True


```

---
## Заключение

#### Представленные решения демонстрируют разные подходы к решению задач с использованием функционального программирования в haskell.


---

# Difference Between `foldl` and `foldr` in Haskell

In Haskell, `foldl` and `foldr` are two functions used to perform folding operations on lists (or other foldable data structures). They are very similar in purpose, but differ in how they process the list, especially in terms of the direction in which they operate. Here's an explanation of the differences between them.

## 1. `foldl` (fold left)

- **Definition**: `foldl` stands for "fold from the left". It starts folding the list from the left-most element, applying the function cumulatively to the list.
- **How it works**: The function starts from the first element and applies the folding function to it, then moves on to the next element, and so on, until the end of the list.

### Signature:
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
```

### Example:

```haskell
sumList :: [Int] -> Int
sumList = foldl (+) 0
```

```haskell
sumList [1, 2, 3, 4]
-- 0 + 1 = 1
-- 1 + 2 = 3
-- 3 + 3 = 6
-- 6 + 4 = 10

```
## 2. `foldr` (fold right)

### Signature:
```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

### Example:

```haskell
doubleAndSumList :: [Int] -> Int
doubleAndSumList = foldr (\x acc -> (x * 2) + acc) 0
```

```haskell
doubleAndSumList [1, 2, 3, 4]
-- 4 * 2 + 0 = 8
-- 3 * 2 + 8 = 14
-- 2 * 2 + 14 = 18
-- 1 * 2 + 18 = 20


```
