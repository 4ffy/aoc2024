#!/usr/bin/env python
import math
import sys
import os


def digits(x):
    return int(math.log10(x)) + 1


def split(x):
    n = 10 ** (digits(x) / 2)
    return [int(x / n), int(x % n)]


cache = {0: [1]}


def blink(lst):
    result = []
    for x in lst:
        if x in cache:
            result.extend(cache[x])
        elif digits(x) & 1 == 0:
            cache[x] = split(x)
            result.extend(cache[x])
        else:
            cache[x] = [x * 2024]
            result.extend(cache[x])
    return result


if __name__ == "__main__":
    sys.argv = ["", "input"]
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)
    data = None
    with open(sys.argv[1], "rt") as f:
        data = f.read().rstrip()
    lst = [int(x) for x in data.split(" ")]
    for i in range(25):
        lst = blink(lst)
    print(f"Stones: {len(lst)}")
