import unittest
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
