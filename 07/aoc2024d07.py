#!/usr/bin/env python
import itertools as it
import math
import os
import sys
from multiprocessing import Pool


def is_valid(round):
    expected, operands = round
    for i in range(1 << (len(operands) - 1)):
        result = operands[0]
        for j in range(len(operands) - 1):
            if i & (1 << j):
                result += operands[j + 1]
            else:
                result *= operands[j + 1]
        if result == expected:
            return True
    return False


def sum_valid(data):
    with Pool() as p:
        result = p.map(is_valid, data)
        return sum(data[x][0] for x in range(len(result)) if result[x])


def concat(x, y):
    y_digits = int(math.log10(y)) + 1 if y > 0 else 1
    return x * 10**y_digits + y


def is_valid_concat(round):
    expected, operands = round
    for seq in it.product((0, 1, 2), repeat=len(operands) - 1):
        result = operands[0]
        for i, op in enumerate(seq):
            if op == 0:
                result += operands[i + 1]
            elif op == 1:
                result *= operands[i + 1]
            else:
                result = concat(result, operands[i + 1])
        if result == expected:
            return True
    return False


def sum_valid_concat(data):
    with Pool() as p:
        result = p.map(is_valid_concat, data)
        return sum(data[x][0] for x in range(len(result)) if result[x])


def parse(src):
    result = []
    for line in src.split("\n"):
        expected, operands = line.split(": ")
        expected = int(expected)
        operands = [int(x) for x in operands.split(" ")]
        result.append((expected, operands))
    return result


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'.", file=sys.stderr)
        exit(1)
    data = None
    with open(sys.argv[1], "rt") as f:
        data = f.read().rstrip()
    db = parse(data)
    print(f"Sum of valid: {sum_valid(db)}")
    print(f"Sum of concat: {sum_valid_concat(db)}")
