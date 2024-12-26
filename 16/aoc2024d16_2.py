#!/usr/bin/env python
import sys
import os
from collections import UserDict
from copy import deepcopy
from queue import PriorityQueue

North = 0
South = 1
West = 2
East = 3


class Cell:
    def __init__(self):
        self.neighbors = set()
        self.visited = False
        self.dist = float("inf")


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
                if (y, x, 0) not in self.data:
                    result.append("#")
                else:
                    result.append(".")
            result.append("\n")
        return "".join(result)

    @staticmethod
    def from_string(src):
        lines = src.split("\n")
        data = {}
        height = len(lines)
        width = len(lines[0])
        start_pos = None
        end_pos = None
        for y, line in enumerate(lines):
            for x, char in enumerate(line):
                if char != "#":
                    if char == "S":
                        start_pos = (y, x)
                    if char == "E":
                        end_pos = (y, x)

                    data[(y, x, North)] = Cell()
                    data[(y, x, North)].neighbors.add(((y, x, West), 1000))
                    data[(y, x, North)].neighbors.add(((y, x, East), 1000))
                    if y > 0 and lines[y - 1][x] != "#":
                        data[(y, x, North)].neighbors.add(
                            ((y - 1, x, North), 1)
                        )

                    data[(y, x, South)] = Cell()
                    data[(y, x, South)].neighbors.add(((y, x, West), 1000))
                    data[(y, x, South)].neighbors.add(((y, x, East), 1000))
                    if y < height - 1 and lines[y + 1][x] != "#":
                        data[(y, x, South)].neighbors.add(
                            ((y + 1, x, South), 1)
                        )

                    data[(y, x, West)] = Cell()
                    data[(y, x, West)].neighbors.add(((y, x, North), 1000))
                    data[(y, x, West)].neighbors.add(((y, x, South), 1000))
                    if x > 0 and lines[y][x - 1] != "#":
                        data[(y, x, West)].neighbors.add(((y, x - 1, West), 1))

                    data[(y, x, East)] = Cell()
                    data[(y, x, East)].neighbors.add(((y, x, North), 1000))
                    data[(y, x, East)].neighbors.add(((y, x, South), 1000))
                    if x < width - 1 and lines[y][x + 1] != "#":
                        data[(y, x, East)].neighbors.add(((y, x + 1, East), 1))

        return Grid(data, height, width, start_pos, end_pos)

    def _dijkstra(self, start):
        for cell in self.data.values():
            cell.visited = False
            cell.dist = float("Inf")
        self.data[start].dist = 0

        queue = PriorityQueue()
        queue.put(QueueItem(start, self.data[start]))
        while not queue.empty():
            curr = queue.get()
            y, x, dir = curr.pos
            cell = curr.cell
            cell.visited = True
            for (y_2, x_2, dir_2), dist in cell.neighbors:
                neighbor = self.data[(y_2, x_2, dir_2)]
                if cell.dist + dist < neighbor.dist:
                    neighbor.dist = cell.dist + dist
                    queue.put(
                        QueueItem(
                            (y_2, x_2, dir_2), self.data[(y_2, x_2, dir_2)]
                        )
                    )

    def shortest_path(self):
        self._dijkstra((*self.start, East))
        return min(
            self.data[(*self.end, North)].dist,
            self.data[(*self.end, South)].dist,
            self.data[(*self.end, West)].dist,
            self.data[(*self.end, East)].dist,
        )

    def cells_visited(self):
        result = set()
        shortest = self.shortest_path()
        copy = deepcopy(self)
        for dir in (North, South, West, East):
            copy._dijkstra((*self.end, dir))
            for (y, x, dir), cell in self.items():
                flip = -1

                if dir == North:
                    flip = South
                elif dir == South:
                    flip = North
                elif dir == West:
                    flip = East
                else:
                    flip = West

                if copy[(y, x, flip)].dist + cell.dist == shortest:
                    result.add((y, x))

        return len(result)


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
    grid = Grid.from_string(data)
    print(f"Shortest path: {grid.shortest_path()}")
    print(f"Ideal cells: {grid.cells_visited()}")
