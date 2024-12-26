#!/usr/bin/env python
import itertools as it
import sys
import os


def parse_grid(src):
    result = [None for x in range(5)]
    for i, line in enumerate(src.split("\n")):
        for j, char in enumerate(line):
            if char == "." and result[j] is None:
                result[j] = i - 1
    return tuple(result)


def parse_data(src):
    locks = []
    keys = []
    grids = src.split("\n\n")
    for grid in grids:
        if grid[0] == "#":
            locks.append(parse_grid(grid))
        else:
            keys.append(parse_grid(grid[::-1])[::-1])
    return locks, keys


def is_valid(lock, key):
    for i, j in zip(lock, key):
        if i + j > 5:
            return False
    return True


def count_valid(locks, keys):
    sum = 0
    for key, lock in it.product(locks, keys):
        if is_valid(lock, key):
            sum += 1
    return sum


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
    locks, keys = parse_data(data)
    print(f"Valid pairs: {count_valid(locks, keys)}")
