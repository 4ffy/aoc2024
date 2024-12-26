#!/usr/bin/env python
import sys
import os
from collections import UserDict

North = 1
South = 2
West = 4
East = 8


class Cell:
    def __init__(self):
        self.neighbors = 0
        self.visited = False
        self.dist = float("inf")
        self.parent = 0

    def __str__(self):
        return f"{self.neighbors} {self.visited} {self.dist}"


class Grid(UserDict):
    def __init__(self, data, height, width, start_pos, end_pos):
        self.data = data
        self.height = height
        self.width = width
        self.start = start_pos
        self.end = end_pos

    def __str__(self):
        result = []
        dir_map = {1: "N", 2: "S", 4: "W", 8: "E"}
        for y in range(self.height):
            for x in range(self.width):
                cell = self.data[(y, x)]
                dirs = []
                for dir in (North, South, West, East):
                    if cell.parent & dir:
                        dirs += dir_map[cell.parent & dir]
                dirs = "".join(dirs)
                result.append(f"{cell.dist:5}({dirs:2}) ")
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

    def dijkstra(self, start, end, start_dir):
        queue = []

        for y in range(self.height):
            for x in range(self.width):
                self.data[(y, x)].visited = False
                self.data[(y, x)].dist = float("Inf")
                queue.append((y, x))
        self.data[start].dist = 0

        if start_dir == North:
            self.data[start].parent = South
        if start_dir == South:
            self.data[start].parent = North
        if start_dir == West:
            self.data[start].parent = East
        if start_dir == East:
            self.data[start].parent = West

        while self.data[end].dist == float("Inf"):
            queue = sorted(queue, key=lambda x: self.data[x].dist, reverse=True)
            curr = queue.pop()
            y, x = curr
            cell = self.data[curr]
            cell.visited = True
            dist = 0

            print(y, x)
            print(self)

            if cell.neighbors & North and not self.data[(y - 1, x)].visited:
                if cell.parent == South:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002

                if dist <= self.data[(y - 1, x)].dist:
                    self.data[(y - 1, x)].dist = dist
                    self.data[(y - 1, x)].parent |= South

            if cell.neighbors & South and not self.data[(y + 1, x)].visited:
                if cell.parent == North:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002

                if dist <= self.data[(y + 1, x)].dist:
                    self.data[(y + 1, x)].dist = dist
                    self.data[(y + 1, x)].parent |= North

            if cell.neighbors & West and not self.data[(y, x - 1)].visited:
                if cell.parent == East:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002
                if dist <= self.data[(y, x - 1)].dist:
                    self.data[(y, x - 1)].dist = dist
                    self.data[(y, x - 1)].parent |= East

            if cell.neighbors & East and not self.data[(y, x + 1)].visited:
                if cell.parent == West:
                    dist = cell.dist + 2
                else:
                    dist = cell.dist + 1002

                if dist <= self.data[(y, x + 1)].dist:
                    self.data[(y, x + 1)].dist = dist
                    self.data[(y, x + 1)].parent |= West

        return self.data[end].dist

    def shortest_count(self, y, x):
        for cell in self.data.values():
            cell.visited = False


if __name__ == "__main__":
    sys.argv = ["", "input2"]
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
    print(f"Shortest path: {grid.dijkstra(grid.start, grid.end, East)}")
    print(grid)
    print(data)
    count = 0
    print(f"Number of tiles: {count}")
