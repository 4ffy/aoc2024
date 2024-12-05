#!/bin/env python
import os
import re
import sys

regex = r"do\(\)|don't\(\)|mul\(\d{1,3},\d{1,3}\)"


def mul(a, b):
    return a * b


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)

    with open(sys.argv[1], "rt") as f:
        data = f.read()
        tokens = re.findall(regex, data)

        part1_sum = 0
        part2_sum = 0
        disabled = False
        for token in tokens:
            if token == "do()":
                disabled = False
            elif token == "don't()":
                disabled = True
            else:  # mul
                prod = eval(token)
                part1_sum += prod
                if not disabled:
                    part2_sum += prod

        print(f"Part 1: {part1_sum}")
        print(f"Part 2: {part2_sum}")
