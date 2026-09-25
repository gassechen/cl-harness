"""Growth stages for the cl-harness context experiment.

Usage: python3 stage.py <stage>

Every stage is idempotent: running it twice never duplicates code. The
harness agent only executes these stages through exec_command, so project
growth is deterministic while the LLM context still grows naturally
(user input, command output, file reads, responses).

Stages: gcd, is_perfect_square, sum_range, prime_factors, text_utils, main_demo
"""
import os
import re
import sys

MATH_STAGES = {
    "gcd": (
        "def gcd(",
        "gcd",
        "TestGcd",
        '''
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
''',
        '''
class TestGcd(unittest.TestCase):
    def test_gcd_base(self):
        self.assertEqual(gcd(0, 0), 0)
        self.assertEqual(gcd(5, 0), 5)
        self.assertEqual(gcd(0, 7), 7)

    def test_gcd_typical(self):
        self.assertEqual(gcd(12, 18), 6)
        self.assertEqual(gcd(100, 75), 25)
        self.assertEqual(gcd(17, 5), 1)

    def test_gcd_negative(self):
        self.assertEqual(gcd(-12, 18), 6)
        self.assertEqual(gcd(-12, -18), 6)
''',
    ),
    "is_perfect_square": (
        "def is_perfect_square(",
        "is_perfect_square",
        "TestIsPerfectSquare",
        '''
def is_perfect_square(n):
    """
    Return True if n is a perfect square, False otherwise.
    Negative numbers are never perfect squares; 0 is.
    """
    if n < 0:
        return False
    root = int(n ** 0.5)
    return root * root == n
''',
        '''
class TestIsPerfectSquare(unittest.TestCase):
    def test_is_perfect_square_base(self):
        self.assertTrue(is_perfect_square(0))
        self.assertTrue(is_perfect_square(1))

    def test_is_perfect_square_typical(self):
        self.assertTrue(is_perfect_square(16))
        self.assertTrue(is_perfect_square(144))
        self.assertTrue(is_perfect_square(1024))

    def test_is_perfect_square_false(self):
        self.assertFalse(is_perfect_square(2))
        self.assertFalse(is_perfect_square(15))
        self.assertFalse(is_perfect_square(-4))
''',
    ),
    "sum_range": (
        "def sum_range(",
        "sum_range",
        "TestSumRange",
        '''
def sum_range(start, end):
    """
    Return the inclusive sum of every integer from start to end.
    Returns 0 when end < start; negative ranges are supported.
    """
    if end < start:
        return 0
    return (start + end) * (end - start + 1) // 2
''',
        '''
class TestSumRange(unittest.TestCase):
    def test_sum_range_base(self):
        self.assertEqual(sum_range(1, 1), 1)
        self.assertEqual(sum_range(0, 0), 0)

    def test_sum_range_typical(self):
        self.assertEqual(sum_range(1, 10), 55)
        self.assertEqual(sum_range(3, 7), 25)

    def test_sum_range_negative(self):
        self.assertEqual(sum_range(-3, 2), -3)
        self.assertEqual(sum_range(-5, -1), -15)

    def test_sum_range_empty(self):
        self.assertEqual(sum_range(5, 4), 0)
''',
    ),
    "prime_factors": (
        "def prime_factors(",
        "prime_factors",
        "TestPrimeFactors",
        '''
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
''',
        '''
class TestPrimeFactors(unittest.TestCase):
    def test_prime_factors_base(self):
        self.assertEqual(prime_factors(0), [])
        self.assertEqual(prime_factors(1), [])

    def test_prime_factors_typical(self):
        self.assertEqual(prime_factors(12), [2, 2, 3])
        self.assertEqual(prime_factors(100), [2, 2, 5, 5])
        self.assertEqual(prime_factors(13), [13])

    def test_prime_factors_repeated(self):
        self.assertEqual(prime_factors(8), [2, 2, 2])
        self.assertEqual(prime_factors(360), [2, 2, 2, 3, 3, 5])
''',
    ),
}

TEXT_UTILS = '''"""Small text helpers used by the math/text demo project."""


def is_palindrome(s):
    """
    Return True if s reads the same forwards and backwards.
    Case and surrounding/internal spaces are ignored.
    """
    cleaned = "".join(s.lower().split())
    return cleaned == cleaned[::-1]


def word_count(s):
    """
    Return the number of whitespace-separated words in s.
    """
    return len(s.split())


def title_case(s):
    """
    Return s with the first letter of every word in upper case.
    """
    return " ".join(w[:1].upper() + w[1:] for w in s.split())
'''

TEST_TEXT_UTILS = '''import unittest
from text_utils import is_palindrome, word_count, title_case


class TestTextUtils(unittest.TestCase):
    def test_is_palindrome_true(self):
        self.assertTrue(is_palindrome("Anita lava la tina"))
        self.assertTrue(is_palindrome("A man a plan a canal Panama"))

    def test_is_palindrome_false(self):
        self.assertFalse(is_palindrome("cl-harness"))
        self.assertFalse(is_palindrome("harness"))
        self.assertTrue(is_palindrome(""))

    def test_word_count(self):
        self.assertEqual(word_count("uno dos tres"), 3)
        self.assertEqual(word_count("  espacios   raros "), 2)
        self.assertEqual(word_count(""), 0)

    def test_title_case(self):
        self.assertEqual(title_case("hola mundo"), "Hola Mundo")
        self.assertEqual(title_case("PYTHON es GENIAL"), "PYTHON Es GENIAL")
'''

MAIN_DEMO = '''from math_utils import (
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
'''


def read(path):
    with open(path, encoding="utf-8") as fh:
        return fh.read()


def write(path, text):
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(text)


def append_once(path, marker, text):
    if marker in read(path):
        return False
    with open(path, "a", encoding="utf-8") as fh:
        fh.write(text)
    return True


def add_math_import(symbol):
    path = "test_math_utils.py"
    src = read(path)
    match = re.search(r"^from math_utils import (.+)$", src, re.M)
    if not match:
        return
    names = [n.strip() for n in match.group(1).split(",")]
    if symbol in names:
        return
    names.append(symbol)
    write(path, src[:match.start()]
          + "from math_utils import " + ", ".join(names)
          + src[match.end():])


def stage_math(name):
    func_marker, symbol, class_marker, func_code, test_code = MATH_STAGES[name]
    added_func = append_once("math_utils.py", func_marker, func_code)
    add_math_import(symbol)
    added_test = append_once("test_math_utils.py", class_marker, test_code)
    print("stage {}: math_utils={} test_math_utils={}".format(
        name, "actualizado" if added_func else "sin cambios",
        "actualizado" if added_test else "sin cambios"))


def stage_text_utils():
    a = not os.path.exists("text_utils.py")
    b = not os.path.exists("test_text_utils.py")
    if a:
        write("text_utils.py", TEXT_UTILS)
    if b:
        write("test_text_utils.py", TEST_TEXT_UTILS)
    print("stage text_utils: text_utils.py={} test_text_utils.py={}".format(
        "creado" if a else "ya existia", "creado" if b else "ya existia"))


def stage_main_demo():
    if "is_perfect_square" in read("main.py"):
        print("stage main_demo: main.py sin cambios")
        return
    write("main.py", MAIN_DEMO)
    print("stage main_demo: main.py actualizado")


def main():
    if len(sys.argv) != 2:
        print("uso: python3 stage.py <" + "|".join(
            list(MATH_STAGES) + ["text_utils", "main_demo"]) + ">")
        return 2
    stage = sys.argv[1]
    if stage in MATH_STAGES:
        stage_math(stage)
    elif stage == "text_utils":
        stage_text_utils()
    elif stage == "main_demo":
        stage_main_demo()
    else:
        print("etapa desconocida: " + stage)
        return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
