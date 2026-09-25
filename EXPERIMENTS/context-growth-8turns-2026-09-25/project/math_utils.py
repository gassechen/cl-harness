def factorial(n):
    """
    Return the factorial of n (n!).
    Raises ValueError for negative inputs.
    """
    if n < 0:
        raise ValueError("Factorial is not defined for negative numbers")
    result = 1
    for i in range(2, n + 1):
        result *= i
    return result


def is_prime(n):
    """
    Return True if n is a prime number, False otherwise.
    Handles n < 2 as non‑prime.
    """
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    i = 3
    while i * i <= n:
        if n % i == 0:
            return False
        i += 2
    return True


def fibonacci(n):
    """
    Return the nth Fibonacci number (0‑indexed).
    Raises ValueError for negative inputs.
    """
    if n < 0:
        raise ValueError("Fibonacci is not defined for negative numbers")
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a
def gcd(a, b):
    """
    Return the greatest common divisor of a and b.
    Iterative Euclidean algorithm; gcd(0, 0) is 0 and negative
    arguments are handled by absolute value.
    """
    a, b = abs(a), abs(b)
    while b:
        a, b = b, a % b
    return a

def is_perfect_square(n):
    """
    Return True if n is a perfect square, False otherwise.
    Negative numbers are never perfect squares; 0 is.
    """
    if n < 0:
        return False
    root = int(n ** 0.5)
    return root * root == n

def sum_range(start, end):
    """
    Return the inclusive sum of every integer from start to end.
    Returns 0 when end < start; negative ranges are supported.
    """
    if end < start:
        return 0
    return (start + end) * (end - start + 1) // 2

def prime_factors(n):
    """
    Return the sorted list of prime factors of n, with multiplicity.
    Returns an empty list for n < 2.
    """
    factors = []
    n = abs(n)
    divisor = 2
    while divisor * divisor <= n:
        while n % divisor == 0:
            factors.append(divisor)
            n //= divisor
        divisor += 1
    if n > 1:
        factors.append(n)
    return factors
