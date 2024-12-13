#!/usr/bin/env python
import math
import numpy as np
import sys
import os
from collections import namedtuple

Game = namedtuple("Game", ["want", "coeffs"])


def parse_line(line, sep="+"):
    numbers = line.split(": ")[1]
    x_str, y_str = numbers.split(", ")
    x = int(x_str.split(sep)[1])
    y = int(y_str.split(sep)[1])
    return x, y


def parse_data(src):
    result = []
    for record in src.split("\n\n"):
        a_line, b_line, prize_line = record.split("\n")
        a_x, a_y = parse_line(a_line)
        b_x, b_y = parse_line(b_line)
        prize_x, prize_y = parse_line(prize_line, "=")
        want = np.array([[prize_x], [prize_y]])
        coeffs = np.array([[a_x, b_x], [a_y, b_y]])
        result.append(Game(want, coeffs))
    return result


def score(game):
    solution = np.linalg.solve(game.coeffs, game.want)
    a_presses = round(solution[0][0], 3)
    b_presses = round(solution[1][0], 3)
    if a_presses.is_integer() and b_presses.is_integer():
        return 3 * int(a_presses) + int(b_presses)
    return 0


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
    games = parse_data(data)

    sum = 0
    for game in games:
        sum += score(game)
    print(f"Minimum tokens: {sum}")

    sum = 0
    for game in games:
        game.want[0][0] += 10000000000000
        game.want[1][0] += 10000000000000
        sum += score(game)
    print(f"Minimum tokens 2: {sum}")
