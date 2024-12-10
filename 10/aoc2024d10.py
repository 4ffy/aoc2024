#!/usr/bin/env python
import sys
import os


def walk(grid, y, x):
    result = 0
    stack = []
    stack.append((y, x))
    while len(stack) > 0:
        y, x = stack.pop()
        score = grid[y][x]
        if score == 9:
            result += 1
            continue
        if y > 0 and grid[y - 1][x] == score + 1:
            stack.append((y - 1, x))
        if y < len(grid) - 1 and grid[y + 1][x] == score + 1:
            stack.append((y + 1, x))
        if x > 0 and grid[y][x - 1] == score + 1:
            stack.append((y, x - 1))
        if x < len(grid[0]) - 1 and grid[y][x + 1] == score + 1:
            stack.append((y, x + 1))
    return result


def walk_tuah(grid, y, x):
    result = set()
    stack = []
    stack.append((y, x))
    while len(stack) > 0:
        y, x = stack.pop()
        score = grid[y][x]
        if score == 9:
            result.add((y, x))
            continue
        if y > 0 and grid[y - 1][x] == score + 1:
            stack.append((y - 1, x))
        if y < len(grid) - 1 and grid[y + 1][x] == score + 1:
            stack.append((y + 1, x))
        if x > 0 and grid[y][x - 1] == score + 1:
            stack.append((y, x - 1))
        if x < len(grid[0]) - 1 and grid[y][x + 1] == score + 1:
            stack.append((y, x + 1))
    return len(result)


def score(grid):
    sum = 0
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] == 0:
                sum += walk_tuah(grid, y, x)
    return sum

def rate(grid):
    sum = 0
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] == 0:
                sum += walk(grid, y, x)
    return sum


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)
    data = None
    with open(sys.argv[1], "rt") as f:
        data = f.read().rstrip()
    grid = [[int(x) for x in line] for line in data.split("\n")]
    print(f"Score: {score(grid)}")
    print(f"Rating: {rate(grid)}")
