#!/usr/bin/env python
import colorsys
import math
import os
import random
import sys
from itertools import combinations_with_replacement as comb


def random_color():
    r, g, b = colorsys.hsv_to_rgb(random.random(), 1.0, 1.0)
    r = int(r * 255)
    g = int(g * 255)
    b = int(b * 255)
    return f"#{r:02x}{g:02x}{b:02x}"


def parse_rule(rule):
    left, right = rule.split("|")
    return int(left), int(right)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)

    # Read file
    data = None
    with open(sys.argv[1]) as f:
        data = f.read()
    rules_section = data.split("\n\n")[0]
    rules = [parse_rule(x) for x in rules_section.split()]

    # Numbers aren't valid symbols in dot so I have to map them to something
    # alphabetic.
    symbols = {}
    for left, right in rules:
        if left not in symbols:
            symbols[left] = ""
        if right not in symbols:
            symbols[right] = ""
    digits = int(math.log(len(symbols)) / math.log(26)) + 1
    name_gen = comb("abcdefghijklmnopqrstuvwxyz", digits)
    for key in symbols:
        symbols[key] = "".join(next(name_gen))

    # Print graph.
    print("digraph {")
    for key, val in symbols.items():
        print(f'    {val} [label="{key}"]')
    for left, right in rules:
        print(
            f'    {symbols[left]} -> {symbols[right]} [color="{random_color()}"]'
        )
    print("}")
