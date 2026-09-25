from math_utils import (
    factorial,
    fibonacci,
    gcd,
    is_perfect_square,
    is_prime,
    prime_factors,
    sum_range,
)
from text_utils import is_palindrome, title_case, word_count


def demo_math():
    print(f"Factorial of 5 is {factorial(5)}")
    print(f"Is 29 a prime? {'Yes' if is_prime(29) else 'No'}")
    print(f"The 10th Fibonacci number is {fibonacci(10)}")
    print(f"gcd(12, 18) = {gcd(12, 18)}")
    print(f"Is 144 a perfect square? {'Yes' if is_perfect_square(144) else 'No'}")
    print(f"sum_range(1, 10) = {sum_range(1, 10)}")
    print(f"prime_factors(360) = {prime_factors(360)}")


def demo_text():
    phrase = "Anita lava la tina"
    print(f"Is '{phrase}' a palindrome? {'Yes' if is_palindrome(phrase) else 'No'}")
    print(f"word_count('{phrase}') = {word_count(phrase)}")
    print(f"title_case('cl harness rocks') = {title_case('cl harness rocks')}")


if __name__ == "__main__":
    demo_math()
    demo_text()
