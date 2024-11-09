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
