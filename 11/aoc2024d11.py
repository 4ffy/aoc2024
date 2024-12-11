#!/usr/bin/env python
import math
import sys
import os


def digits(x):
    return int(math.log10(x)) + 1


def split(x):
    n = 10 ** (digits(x) / 2)
    return int(x / n), int(x % n)


cache = {}


def blink(x, gens_left):
    if gens_left == 0:
        return 1
    if (x, gens_left) in cache:
        return cache[(x, gens_left)]
    result = None
    if x == 0:
        result = blink(1, gens_left - 1)
    elif digits(x) & 1 == 0:
        l, r = split(x)
        result = blink(l, gens_left - 1) + blink(r, gens_left - 1)
    else:
        result = blink(2024 * x, gens_left - 1)
    cache[(x, gens_left)] = result
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

    sum = 0
    for x in lst:
        sum += blink(x, 25)
    print(f"25 blinks: {sum}")

    sum = 0
    for x in lst:
        sum += blink(x, 75)
    print(f"75 blinks: {sum}")
