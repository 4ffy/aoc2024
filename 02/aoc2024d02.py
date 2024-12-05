#!/usr/bin/env python
import itertools as it
import os
import sys


def is_increasing(lst):
    for pair in it.pairwise(lst):
        if pair[1] <= pair[0]:
            return False
    return True


def is_decreasing(lst):
    for pair in it.pairwise(lst):
        if pair[1] >= pair[0]:
            return False
    return True


def is_smooth(lst):
    for pair in it.pairwise(lst):
        if abs(pair[1] - pair[0]) > 3:
            return False
    return True


def is_safe_naive(lst):
    return (is_increasing(lst) or is_decreasing(lst)) and is_smooth(lst)


def is_safe(lst):
    if is_safe_naive(lst):
        return True
    for i in range(len(lst)):
        sublist = lst[:i] + lst[i + 1 :]
        if is_safe_naive(sublist):
            return True
    return False


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'.", file=sys.stderr)
        exit(1)
    with open(sys.argv[1], "rt") as f:
        data = f.read()
        lines = [x for x in data.split("\n") if x != ""]

        naive = 0
        safe = 0
        for line in lines:
            numbers = [int(x) for x in line.split(" ") if x != ""]
            if is_safe_naive(numbers):
                naive += 1
            if is_safe(numbers):
                safe += 1
        print(f"Naive count: {naive}")
        print(f"Safe count: {safe}")
