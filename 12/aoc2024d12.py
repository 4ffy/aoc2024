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

    def get(self, y, x):
        return self.data[y][x]

    def clear_visited(self):
        for row in self.data:
            for item in row:
                item.visited = False

    def is_visited(self, y, x):
        return self.data[y][x].visited

    def visit(self, y, x):
        self.data[y][x].visited = True

    def walk(self, start_y, start_x):
        area = 0
        perimeter = 0
        sides = 0

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
                    queue.put(self.get(y - 1, x))
            else:
                perimeter += 1

            if curr.connections & SOUTH:
                if not self.is_visited(y + 1, x):
                    self.visit(y + 1, x)
                    queue.put(self.get(y + 1, x))
            else:
                perimeter += 1

            if curr.connections & WEST:
                if not self.is_visited(y, x - 1):
                    self.visit(y, x - 1)
                    queue.put(self.get(y, x - 1))
            else:
                perimeter += 1

            if curr.connections & EAST:
                if not self.is_visited(y, x + 1):
                    self.visit(y, x + 1)
                    queue.put(self.get(y, x + 1))
            else:
                perimeter += 1

            # Concave north/east
            if not curr.connections & NORTH and not curr.connections & EAST:
                sides += 1

            # Concave south/west
            if not curr.connections & SOUTH and not curr.connections & WEST:
                sides += 1

            # Concave east/south
            if not curr.connections & EAST and not curr.connections & SOUTH:
                sides += 1

            # Concave west/north
            if not curr.connections & WEST and not curr.connections & NORTH:
                sides += 1

            # Convex north/west
            if (
                curr.connections & NORTH
                and curr.connections & WEST
                and not self.get(y - 1, x).connections & WEST
                and not self.get(y, x - 1).connections & NORTH
            ):
                sides += 1

            # Convex south/east
            if (
                curr.connections & SOUTH
                and curr.connections & EAST
                and not self.get(y + 1, x).connections & EAST
                and not self.get(y, x + 1).connections & SOUTH
            ):
                sides += 1

            # Convex east/north
            if (
                curr.connections & EAST
                and curr.connections & NORTH
                and not self.get(y, x + 1).connections & NORTH
                and not self.get(y - 1, x).connections & EAST
            ):
                sides += 1

            # Convex west/south
            if (
                curr.connections & WEST
                and curr.connections & SOUTH
                and not self.get(y, x - 1).connections & SOUTH
                and not self.get(y + 1, x).connections & WEST
            ):
                sides += 1

        return area, perimeter, sides

    def price(self):
        sum = 0
        bulk = 0
        self.clear_visited()
        for y in range(self.height):
            for x in range(self.width):
                if self.is_visited(y, x):
                    continue
                area, perimeter, sides = self.walk(y, x)
                sum += area * perimeter
                bulk += area * sides
        return sum, bulk


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
    price, bulk = grid.price()
    print(f"Normal cost: {price}")
    print(f"Bulk cost: {bulk}")
