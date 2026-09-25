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
