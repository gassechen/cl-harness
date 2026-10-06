import json

def merge_intervals(intervals):
    intervals.sort()
    merged = []
    for interval in intervals:
        if not merged or merged[-1][1] < interval[0]:
            merged.append(interval)
        else:
            merged[-1][1] = max(merged[-1][1], interval[1])
    return merged

def analyze_intervals(intervals):
    merged = merge_intervals(intervals)
    min_val = min(interval[0] for interval in merged)
    max_val = max(interval[1] for interval in merged)
    gaps = [i for i in range(min_val, max_val + 1) if all(i not in interval for interval in merged)]
    overlaps = {i: 0 for i in range(min_val, max_val + 1)}
    for interval in merged:
        for i in range(interval[0], interval[1] + 1):
            overlaps[i] += 1
    return merged, gaps, overlaps

def main():
    with open('intervals.json', 'r') as f:
        intervals = json.load(f)
    merged, gaps, overlaps = analyze_intervals(intervals)
    print('Merged Intervals:', merged)
    print('Gaps:', gaps)
    print('Overlaps:', overlaps)

if __name__ == '__main__':
    main()
