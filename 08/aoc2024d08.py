#!/usr/bin/env python3
import itertools as it
import os
import sys
from collections import namedtuple, UserDict


Point = namedtuple("Point", ["y", "x"])


class Grid(UserDict):
    def __init__(self, points, height, width):
        self.data = points
        self.height = height
        self.width = width

    def in_bounds(self, point):
        return 0 <= point.y < self.height and 0 <= point.x < self.width

    def find_antinodes(self):
        result = set()
        for char, points in self.items():
            for p1, p2 in it.combinations(points, 2):
                dy = p2.y - p1.y
                dx = p2.x - p1.x
                if self.in_bounds(antinode_1 := Point(p1.y - dy, p1.x - dx)):
                    result.add(antinode_1)
                if self.in_bounds(antinode_2 := Point(p2.y + dy, p2.x + dx)):
                    result.add(antinode_2)
        return result

    def find_antinodes_2(self):
        result = set()
        for char, points in self.items():
            for p1, p2 in it.combinations(points, 2):
                dy = p2.y - p1.y
                dx = p2.x - p1.x
                i = 0
                done = False
                while not done:
                    done = True
                    if self.in_bounds(
                        antinode_1 := Point(p1.y - i * dy, p1.x - i * dx)
                    ):
                        done = False
                        result.add(antinode_1)
                    if self.in_bounds(
                        antinode_2 := Point(p2.y + i * dy, p2.x + i * dx)
                    ):
                        done = False
                        result.add(antinode_2)
                    i += 1
        return result

    def __str__(self):
        result = [["." for x in range(self.width)] for y in range(self.height)]
        for char, points in self.items():
            for point in points:
                result[point.y][point.x] = char
        return "\n".join("".join(x) for x in result)


def parse_data(src):
    result = {}
    lines = src.split("\n")
    height = len(lines)
    width = len(lines[0])
    for y, line in enumerate(lines):
        for x, char in enumerate(line):
            if char.isalpha() or char.isdigit():
                if char in result:
                    result[char].append(Point(y, x))
                else:
                    result[char] = [Point(y, x)]
    return Grid(result, height, width)


if __name__ == "__main__":
    sys.argv = ["", "input"]
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'")
    data = None
    with open(sys.argv[1], "rt") as f:
        data = f.read().rstrip()
    grid = parse_data(data)
    print(f"Unique antinodes: {len(grid.find_antinodes())}")
    print(f"Unique antinodes 2: {len(grid.find_antinodes_2())}")
