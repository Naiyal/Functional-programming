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
