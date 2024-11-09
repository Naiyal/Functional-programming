# Лабораторная работа №1

**Студент:** Наял М.фахд 
**ИСУ:** 336192  
**Группа:** P3310
**Университет:** НИУ ИТМО  
**Факультет:** Программная инженерия и компьютерная техника  
**Курс:** 3-й курс



**Вариант задач:**
1. Найти сумму всех простых чисел ниже двух миллионов.
2. Найти сумму всех дружественных чисел ниже 10000.


---


## Описание проблемы

### В этом проекте представлено несколько решений для задач с сайта Project Euler. Решения реализованы на языке Haskell. 

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

**Haskell не имеет обычного цикла for, однако можно использовать do-нотацию для списочных вычислений:**

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
testAmicableNumbers :: Test
testAmicableNumbers = TestList [
    TestCase (assertEqual "amicableNumbersTail 10000" (amicableNumbersTail 10000) 31626),
    TestCase (assertEqual "amicableNumbersRecursion 10000" (amicableNumbersRecursion 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersModular 10000" (sumAmicableNumbersModular 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersMap 10000" (sumAmicableNumbersMap 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersDo 10000" (sumAmicableNumbersDo 10000) 31626),
    TestCase (assertEqual "amicableNumbersLazy 10000" (amicableNumbersLazy 10000) 31626)
    ]

```

---
## Заключение

### Представленные решения демонстрируют разные подходы к решению задач с использованием функционального программирования в haskell. Эти примеры показывают, как рекурсия, циклы и функции высшего порядка могут быть использованы для эффективного решения математических задач.


