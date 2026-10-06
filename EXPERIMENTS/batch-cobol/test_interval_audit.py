import unittest
import json
from interval_audit import merge_intervals

class TestIntervalAudit(unittest.TestCase):
    def test_merge_intervals(self):
        intervals = [[1, 3], [2, 6], [8, 10], [15, 18]]
        expected = [[1, 6], [8, 10], [15, 18]]
        self.assertEqual(merge_intervals(intervals), expected)

    def test_detect_gaps(self):
        intervals = [[1, 3], [5, 7]]
        expected = [4]
        self.assertEqual(detect_gaps(intervals), expected)

    def test_count_overlaps(self):
        intervals = [[1, 5], [2, 6], [3, 7]]
        expected = 3
        self.assertEqual(count_overlaps(intervals), expected)

if __name__ == '__main__':
    unittest.main()
