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



