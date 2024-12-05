#!/usr/bin/env python
import os
import re
import sys


def count(seq, item):
    count = 0
    for x in seq:
        if x == item:
            count += 1
    return count


def distance(list_1, list_2):
    sum = 0
    for x, y in zip(list_1, list_2):
        sum += abs(x - y)
    return sum


def similarity(list_1, list_2):
    sum = 0
    for x in list_1:
        sum += x * count(list_2, x)
    return sum


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)

    try:
        with open(sys.argv[1], "rt") as f:
            data = f.read()
            numbers = [int(x) for x in re.split(r"\W+", data) if x != ""]
            left = list(
                sorted([x[1] for x in enumerate(numbers) if x[0] & 1 == 0])
            )
            right = list(
                sorted([x[1] for x in enumerate(numbers) if x[0] & 1 == 1])
            )
            print(f"Distance: {distance(left, right)}")
            print(f"Similarity: {similarity(left, right)}")

    except Exception as e:
        print(f"Could not read '{sys.argv[1]}': {e}.")
