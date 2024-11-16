# Лабораторная работа №2

**Студент:** Наял М.фахд 
**ИСУ:** 336192  
**Группа:** P3310
**Университет:** НИУ ИТМО  
**Факультет:** Программная инженерия и компьютерная техника  
**Курс:** 3-й курс


## Описание решения
Введем абстрактный тип данных -- словарь. Словарь представлен классом типов `Dictionary` у которого есть следующие функции:
- создание пустого словаря;
- получение количества элементов словаря;
- вставка/обновление, удаление, поиск элементов;
- свертки, фильтрация, отображение;
- провекра вхождения элемента в словарь и проверка равенства словарей.


Для реализации словаря создал тип `HashMap`. Эта струтктура данных основана на `Data.Vector`, который позволяет искать элементы по индексу за $O(1)$. 


---

## HashMap

```haskell

module HashMap 
(
    Bucket, 
    HashMap,
    emptyHashMap,
    clearHashMap,
    toList,
    fromList,
    countElements,
    countBuckets,
    lookupH,
    insertH,
    concatH,
    deleteH,
    filterV,
    mapV,
    foldrH,
    foldlH,
    mapH,
    filterH,
    example,
    example2,

    someFunc,
)
where

import Data.Hashable (Hashable, hash)
import Data.List (foldl')
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), choose, listOf)

import Dict

someFunc :: IO ()
someFunc = putStrLn "someFunc"





type Bucket k v = [(k, v)]


data HashMap k v = HashMap {buckets :: Vector (Bucket k v), elementsCount :: Int}
    deriving (Show)


instance (Hashable k) => Semigroup (HashMap k v) where
    (<>) hm1 hm2 = concatH (max (countBuckets hm1) (countBuckets hm2)) hm1 hm2 


instance (Hashable k) => Monoid (HashMap k v) where
    mempty = emptyHashMap 11


instance Dictionary HashMap where
    emptyD = emptyHashMap 11
    elementsCountD = elementsCount

    lookupD = lookupH
    insertD = insertH
    deleteD = deleteH

    foldrD = foldrH
    foldlD = foldlH
    mapD = mapH
    filterD = filterH


instance (Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (HashMap k v) where
  arbitrary = do
    bCount <- choose (1, 23)
    pairs <- listOf ((,) <$> arbitrary <*> arbitrary)
    return $ fromList bCount pairs 
        



emptyHashMap :: Int -> HashMap k v 
emptyHashMap bucketsCount = HashMap {
                                        buckets = V.replicate bucketsCount [],
                                        elementsCount = 0
                                    }


clearHashMap :: HashMap k v -> HashMap k v 
clearHashMap hashMap = emptyHashMap $ countBuckets hashMap


hashToIndex :: (Integral a) => a -> a -> a 
hashToIndex hashCode size = abs hashCode `mod` size


toList :: HashMap k v -> [(k, v)]
toList hashMap = concat $ V.toList $ buckets hashMap


fromList :: (Hashable k) => Int -> [(k, v)] -> HashMap k v 
fromList bucketsCount lst = fromListRec lst (emptyHashMap bucketsCount)
    where
    fromListRec [] hashMap = hashMap
    fromListRec ( (k, v) : xs ) hashMap = fromListRec xs (insertH k v hashMap)


countElements :: Vector (Bucket k v) -> Int
countElements bucketsVector = length $ concat $ V.toList bucketsVector


countBuckets :: HashMap k v -> Int 
countBuckets hashMap = length $ buckets hashMap



lookupH :: (Hashable k) => k -> HashMap k v -> Maybe v 
lookupH k hashMap =  if null res then Nothing else Just (snd $ head res)
    where
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    bucket = buckets hashMap ! index
    res = filter (\(k1, _) -> k1 == k) bucket 



insertH :: Hashable k => k -> v -> HashMap k v -> HashMap k v
insertH k v hashMap = HashMap {
                                buckets = buckets hashMap // [(index, newBucket)],
                                elementsCount = elementsCount hashMap + deltaLength
                            }
    where 
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    oldBucket = buckets hashMap ! index
    newBucket = (k, v) : filter (\(kx, _) -> kx /= k) oldBucket
    deltaLength = length newBucket - length oldBucket


concatH :: Hashable k => Int -> HashMap k v -> HashMap k v -> HashMap k v 
concatH bucketsCount hm1 hm2  = fromList bucketsCount (toList hm1 ++ toList hm2)


deleteH :: Hashable k => k -> HashMap k v -> HashMap k v
deleteH k hashMap = HashMap {
                                buckets = buckets hashMap // [(index, newBucket)],
                                elementsCount = elementsCount hashMap + deltaLength
                            }
    where 
    index = hashToIndex (hash k) (V.length $ buckets hashMap)
    oldBucket = buckets hashMap ! index
    newBucket = filter (\(kx, _) -> kx /= k) oldBucket
    deltaLength = length newBucket - length oldBucket


filterV :: (v -> Bool) -> HashMap k v -> HashMap k v
filterV predic hashMap = HashMap {
                                    buckets = newBuckets,
                                    elementsCount = countElements newBuckets
                                 }
    where    
    newBuckets = filterBuckets $ buckets hashMap

    filterOneBucket = filter (predic . snd)
    
    filterBuckets bucketsVector | V.null bucketsVector = V.empty 
                                | otherwise = filterOneBucket (V.head bucketsVector) `V.cons` filterBuckets (V.tail bucketsVector)


mapV :: (v -> w) -> HashMap k v -> HashMap k w
mapV func hashMap = HashMap {
                                buckets = newBuckets,
                                elementsCount = elementsCount hashMap
                            }
    where
    newBuckets = mapBuckets $ buckets hashMap

    mapOneBucket = map (\(k, v) -> (k, func v))
    
    mapBuckets bucketsVector | V.null bucketsVector = V.empty 
                             | otherwise = mapOneBucket (V.head bucketsVector) `V.cons` mapBuckets (V.tail bucketsVector)


foldrH :: ((k, v) -> b -> b) -> b -> HashMap k v -> b
foldrH func ini hashMap = foldr func ini $ toList hashMap


foldlH :: (b -> (k, v) -> b) -> b -> HashMap k v -> b
foldlH func ini hashMap = foldl' func ini $ toList hashMap


mapH :: Hashable k2 => ((k1, v1) -> (k2, v2)) -> HashMap k1 v1 -> HashMap k2 v2
mapH func hashMap = foldrH (uncurry insertH . func) (emptyHashMap $ V.length $ buckets hashMap) hashMap


filterH :: Hashable k => ((k, v) -> Bool) -> HashMap k v -> HashMap k v
filterH predic hashMap = foldrH (\el hm -> if predic el then uncurry insertH el hm else hm) (clearHashMap hashMap) hashMap



example :: HashMap Int String 
example = insertH 3 "D" $ insertH 2 "C" $ insertH 11 "B" $ insertH 1 "A" $ emptyHashMap 10   

example2 :: HashMap Int Int 
example2 = insertH 3 333 $ insertH 2 22 $ insertH 11 1 $ insertH 1 111 $ emptyHashMap 10   

```


---


## Тестирвоание


```
lab2                > test (suite: lab2-test)
Dictionary Tests
  Unit tests
    Insert test:                          OK
      Preparing...
      Elements count check
      Lookup inserted check
      Lookup not inserted check
    Replace test:                         OK
      Preparing...
      Elements count check
      Lookup inserted check
      Replace inserted..
      Elements count check
      Lookup replaced check
    Delete test:                          OK
      Preparing...
      Elements count check
      Lookup inserted check
      Delete elements...
      Elements count check
      Lookup check
      Delete rest elements...
      Elements count check
      Lookup check
  Property tests
    Mempty test:                          OK (0.01s)
      +++ OK, passed 100 tests.
    Concat associativity test:            OK (0.07s)
      +++ OK, passed 100 tests.
    Foldr to list and restore back:       OK
      +++ OK, passed 100 tests.
    Foldl to list length is elemetsCount: OK
      +++ OK, passed 100 tests.
    Filter doesn't increase elemetsCount: OK
      +++ OK, passed 100 tests.
    Map doesn't change elemetsCount:      OK
      +++ OK, passed 100 tests.
All 9 tests passed (0.07s)
lab2                > Test suite lab2-test passed
```

---

## Заключение

В данной лабораторной работе был реализован и протестирован тип данных HashMap (хеш-таблица) на языке программирования Haskell. Реализация включает основные операции, такие как вставка, удаление, поиск и слияние элементов. Кроме того, были использованы типы и абстракции из стандартной библиотеки Haskell, такие как Vector и Hashable, что позволяет эффективно работать с хешированием и коллекциями.

**В ходе работы были реализованы следующие ключевые элементы:**

1. **Тип `HashMap` и операции над ним:**
   - Создание пустой хеш-таблицы (`emptyHashMap`).
   - Вставка элементов (`insertH`), удаление (`deleteH`), поиск (`lookupH`).
   - Операции слияния и конкатенации хеш-таблиц (`concatH`).
   - Преобразования значений в таблице с помощью `mapV` и фильтрация с помощью `filterV`.
   - Сборка элементов в список с помощью `foldrH` и `foldlH`.

2. **Поддержка интерфейса `Dictionary`:**
   Реализованы методы для работы с хеш-таблицей через интерфейс `Dictionary`, что позволяет использовать `HashMap` в качестве словаря и применять стандартные операции, такие как подсчет элементов, поиск, фильтрация и свертка.

3. **Работа с экземплярами `Semigroup` и `Monoid`:**
   Для хеш-таблицы были определены операторы слияния (`<>`) и нейтральный элемент (`mempty`), что позволяет использовать `HashMap` в контексте операций над монойдами и полугруппами. Это делает возможным использование хеш-таблиц в более сложных структурах данных.

4. **Автоматическое тестирование:**
   Для проверки корректности реализации были написаны тесты с использованием библиотеки `QuickCheck`. Были проведены как тесты единичных операций, так и тесты свойств, такие как:
   - Проверка корректности работы с пустыми и не пустыми хеш-таблицами.
   - Проверка свойств слияния хеш-таблиц (ассоциативности).
   - Тестирование свертки элементов (с использованием `foldr` и `foldl`).
   - Проверка свойств фильтрации и преобразования значений, таких как сохранение количества элементов.

5. **Результаты тестирования:**
   Все тесты прошли успешно, включая как юнит-тесты, так и property-тесты, что подтверждает правильность реализации и соблюдение свойств хеш-таблиц. Также были проверены базовые операции на корректность и эффективность.

## Выводы:
В результате работы была успешно реализована эффективная структура данных HashMap с поддержкой всех основных операций, характерных для хеш-таблиц, таких как вставка, удаление, поиск и слияние. Кроме того, была доказана корректность реализации с помощью автоматических тестов, что подтверждает правильность работы структуры и ее свойств.

Эта лабораторная работа позволяет глубже понять принципы работы хеш-таблиц, особенности их реализации в языке Haskell и применение абстракций для упрощения разработки и тестирования таких структур данных.

