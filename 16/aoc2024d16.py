#!/usr/bin/env python
import sys
import os
from collections import UserDict
from enum import Enum
from queue import PriorityQueue


North = 1
South = 2
West = 4
East = 8


class Cell:
    def __init__(self):
        self.neighbors = 0
        self.visited = False
        self.dist = float("inf")

    def __str__(self):
        return f"{self.neighbors} {self.visited} {self.dist}"


class QueueItem:
    def __init__(self, pos, cell):
        self.pos = pos
        self.cell = cell

    def __lt__(self, other):
        return self.cell.dist < other.cell.dist


class Grid(UserDict):
    def __init__(self, data, height, width, start_pos, end_pos):
        self.data = data
        self.height = height
        self.width = width
        self.start = start_pos
        self.end = end_pos

    def __str__(self):
        result = []
        for y in range(self.height):
            for x in range(self.width):
                result.append(f"{self.data[(y,x)].dist:5} ")
            result.append("\n")
        return "".join(result)

    @staticmethod
    def from_string(src):
        lines = src.split("\n")
        data = {}
        height = len(lines) // 2
        width = len(lines[0]) // 2
        start_pos = None
        end_pos = None
        for y in range(height):
            for x in range(width):
                cell = Cell()
                src_y = 2 * y + 1
                src_x = 2 * x + 1
                assert lines[src_y][src_x] in ".ES"
                if lines[src_y][src_x] == "S":
                    start_pos = (y, x)
                if lines[src_y][src_x] == "E":
                    end_pos = (y, x)
                if lines[src_y - 1][src_x] in ".ES":
                    cell.neighbors |= North
                if lines[src_y + 1][src_x] in ".ES":
                    cell.neighbors |= South
                if lines[src_y][src_x - 1] in ".ES":
                    cell.neighbors |= West
                if lines[src_y][src_x + 1] in ".ES":
                    cell.neighbors |= East
                data[(y, x)] = cell
        return Grid(data, height, width, start_pos, end_pos)

    def dijkstra(self):
        queue = []

        temp = Cell()
        temp.neighbors |= East
        temp.visited = True
        self.data[self.start].neighbors |= West
        self.data[(self.start[0], self.start[1] - 1)] = temp
        queue.append((self.start[0], self.start[1] - 1, temp))

        for y in range(self.height):
            for x in range(self.width):
                self.data[(y, x)].visited = False
                self.data[(y, x)].dist = float("Inf")
                if (y, x) == self.start:
                    self.data[(y, x)].dist = 0
                queue.append((y, x, self.data[y, x]))
        while not self.data[self.end].visited:
            queue = sorted(queue, key=lambda x: x[2].dist, reverse=True)
            curr = queue.pop()
            y, x = curr[:2]
            cell = curr[-1]
            cell.visited = True

            if y == 3 and x == 6:
                print(y, x)
                print(self)

            if y == 4 and x == 7:
                print(y, x)
                print(self)

            dist = 0
            if cell.neighbors & North and not self.data[(y - 1, x)].visited:
                if cell.neighbors & South and self.data[(y + 1, x)].visited:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002
                if dist < self.data[(y - 1, x)].dist:
                    self.data[(y - 1, x)].dist = dist

            if cell.neighbors & South and not self.data[(y + 1, x)].visited:
                if cell.neighbors & North and self.data[(y - 1, x)].visited:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002
                if dist < self.data[(y + 1, x)].dist:
                    self.data[(y + 1, x)].dist = dist

            if cell.neighbors & West and not self.data[(y, x - 1)].visited:
                if cell.neighbors & East and self.data[(y, x + 1)].visited:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002
                if dist < self.data[(y, x - 1)].dist:
                    self.data[(y, x - 1)].dist = dist

            if cell.neighbors & East and not self.data[(y, x + 1)].visited:
                if cell.neighbors & West and self.data[(y, x - 1)].visited:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002
                if dist < self.data[(y, x + 1)].dist:
                    self.data[(y, x + 1)].dist = dist

        return self.data[self.end].dist




if __name__ == "__main__":
    sys.argv = ["", "input3"]
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)
    data = None
    with open(sys.argv[1], "rt") as f:
        data = f.read().rstrip()
    grid = Grid.from_string(data)
    print(data)
    print(f"Shortest path: {grid.dijkstra()}")
    print(grid)
