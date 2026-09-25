import unittest
from math_utils import factorial, is_prime, fibonacci

class TestMathUtils(unittest.TestCase):
    # Factorial tests
    def test_factorial_base(self):
        self.assertEqual(factorial(0), 1)
        self.assertEqual(factorial(1), 1)

    def test_factorial_typical(self):
        self.assertEqual(factorial(5), 120)
        self.assertEqual(factorial(7), 5040)

    def test_factorial_negative(self):
        with self.assertRaises(ValueError):
            factorial(-3)

    # is_prime tests
    def test_is_prime_edge(self):
        self.assertFalse(is_prime(0))
        self.assertFalse(is_prime(1))
        self.assertFalse(is_prime(-5))

    def test_is_prime_primes(self):
        self.assertTrue(is_prime(2))
        self.assertTrue(is_prime(3))
        self.assertTrue(is_prime(13))
        self.assertTrue(is_prime(29))

    def test_is_prime_composites(self):
        self.assertFalse(is_prime(4))
        self.assertFalse(is_prime(9))
        self.assertFalse(is_prime(100))

    # Fibonacci tests
    def test_fibonacci_base(self):
        self.assertEqual(fibonacci(0), 0)
        self.assertEqual(fibonacci(1), 1)

    def test_fibonacci_typical(self):
        self.assertEqual(fibonacci(5), 5)
        self.assertEqual(fibonacci(10), 55)
        self.assertEqual(fibonacci(12), 144)

    def test_fibonacci_negative(self):
        with self.assertRaises(ValueError):
            fibonacci(-4)

if __name__ == '__main__':
    unittest.main()
