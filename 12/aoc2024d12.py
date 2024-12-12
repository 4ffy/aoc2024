#!/usr/bin/env python
import sys
import os
from queue import Queue

# Enums don't work well as bitfields.
NORTH = 1
SOUTH = 2
WEST = 4
EAST = 8


class Cell:
    def __init__(self, char, y, x):
        self.char = char
        self.connections = 0
        self.visited = False
        self.y = y
        self.x = x


class Grid:
    def __init__(self, data, height, width):
        self.data = data
        self.height = height
        self.width = width

    def find_regions(self):
        for y in range(self.height):
            for x in range(self.width):
                curr = self.data[y][x]
                if y > 0 and self.data[y - 1][x].char == curr.char:
                    curr.connections |= NORTH
                    self.data[y - 1][x].connections |= SOUTH
                if (
                    y < self.height - 1
                    and self.data[y + 1][x].connections == curr.char
                ):
                    curr.connections |= SOUTH
                    self.data[y + 1][x].connections |= NORTH
                if x > 0 and self.data[y][x - 1].char == curr.char:
                    curr.connections |= WEST
                    self.data[y][x - 1].connections |= EAST
                if x < self.width - 1 and self.data[y][x + 1].char == curr.char:
                    curr.connections |= EAST
                    self.data[y][x + 1].connections |= WEST

    def is_visited(self, y, x):
        return self.data[y][x].visited

    def visit(self, y, x):
        self.data[y][x].visited = True

    def walk(self, start_y, start_x):
        area = 0
        perimeter = 0
        queue = Queue()
        curr = self.data[start_y][start_x]
        curr.visited = True
        queue.put(curr)
        while not queue.empty():
            curr = queue.get()
            y = curr.y
            x = curr.x
            area += 1

            if curr.connections & NORTH:
                if not self.is_visited(y - 1, x):
                    self.visit(y - 1, x)
                    queue.put(self.data[y - 1][x])
            else:
                perimeter += 1

            if curr.connections & SOUTH:
                if not self.is_visited(y + 1, x):
                    self.visit(y + 1, x)
                    queue.put(self.data[y + 1][x])
            else:
                perimeter += 1

            if curr.connections & WEST:
                if not self.is_visited(y, x - 1):
                    self.visit(y, x - 1)
                    queue.put(self.data[y][x - 1])
            else:
                perimeter += 1

            if curr.connections & EAST:
                if not self.is_visited(y, x + 1):
                    self.visit(y, x + 1)
                    queue.put(self.data[y][x + 1])
            else:
                perimeter += 1

        return area, perimeter

    def price(self):
        sum = 0
        for row in self.data:
            for item in row:
                item.visited = False
        for y in range(self.height):
            for x in range(self.width):
                if self.is_visited(y, x):
                    continue
                area, perimeter = self.walk(y, x)
                sum += area * perimeter
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
        data = [
            [Cell(c, y, x) for x, c in enumerate(line)]
            for y, line in enumerate(f.read().rstrip().split("\n"))
        ]
    grid = Grid(data, len(data), len(data[0]))
    grid.find_regions()
    print(f"Total cost: {grid.price()}")
