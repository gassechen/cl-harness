import unittest
from math_utils import factorial, is_prime, fibonacci, gcd, is_perfect_square, sum_range, prime_factors

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
