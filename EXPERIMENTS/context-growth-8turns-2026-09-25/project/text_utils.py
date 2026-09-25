"""Small text helpers used by the math/text demo project."""


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
