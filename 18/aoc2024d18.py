#!/usr/bin/env python
import sys
import os
from collections import UserDict
from queue import PriorityQueue


def manhattan(p1, p2):
    return abs(p2[0] - p1[0]) + abs(p2[1] - p1[1])


class Tile:
    def __init__(self):
        self.visited = False
        self.start_dist = float("inf")
        self.end_dist = float("inf")


class Grid(UserDict):
    def __init__(self, objects, height, width):
        self.data = objects
        self.height = height
        self.width = width

    def __str__(self):
        result = []
        for y in range(self.height):
            for x in range(self.width):
                if (y, x) in self.data:
                    result.append(".")
                else:
                    result.append("#")
            result.append("\n")
        return "".join(result)

    @staticmethod
    def from_string(src, n):
        points = []
        objects = {}
        lines = src.split("\n")
        for line in lines:
            x, y = (int(n) for n in line.split(","))
            points.append((y, x))
        height = 1 + max(p[0] for p in points)
        width = 1 + max(p[1] for p in points)
        for y in range(height):
            for x in range(width):
                objects[(y, x)] = Tile()
        for i in range(n):
            objects.pop(points[i])
        return Grid(objects, height, width)

    def _queue_weight(self, pos):
        return self.data[pos].start_dist + self.data[pos].end_dist

    def a_star(self, start, end):
        for tile in self.data.values():
            tile.visited = False
            tile.start_dist = float("inf")
            tile.end_dist = float("inf")
        self.data[start].start_dist = 0
        self.data[start].end_dist = manhattan(start, end)

        queue = PriorityQueue()
        queue.put((self._queue_weight(start), start))
        while not queue.empty():
            _, pos = queue.get()
            y, x = pos
            tile = self.data[pos]
            if tile.visited:
                continue
            tile.visited = True
            if (y, x) == end:
                return tile.start_dist

            for dir in [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]:
                if dir in self.data and not self.data[dir].visited:
                    self.data[dir].start_dist = 1 + tile.start_dist
                    self.data[dir].end_dist = manhattan(dir, end)
                    queue.put((self._queue_weight(dir), dir))


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
    grid = Grid.from_string(data, 1024)
    start = (0, 0)
    end = (grid.height - 1, grid.width - 1)
    print(f"Min steps: {grid.a_star(start, end)}")
