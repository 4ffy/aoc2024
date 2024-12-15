#!/usr/bin/env python
import sys
import os
from enum import Enum


class Dirs(Enum):
    North = 0
    South = 1
    West = 2
    East = 3


class Tile(Enum):
    Empty = 0
    Wall = 1
    Box = 2
    Robot = 3


class Grid:
    def __init__(self, objects, height, width, robot_pos):
        self.objects = objects
        self.width = width
        self.height = height
        self.robot_pos = robot_pos

    def __str__(self):
        result = []
        for y in range(self.height):
            for x in range(self.width):
                tile = self.objects[(y, x)]
                if tile == Tile.Empty:
                    result.append(" ")
                elif tile == Tile.Wall:
                    result.append("#")
                elif tile == Tile.Box:
                    result.append("O")
                else:
                    result.append("R")
            result.append("\n")
        return "".join(result)

    def can_move(self, y, x, dir):
        if self.objects[(y, x)] is None or self.objects[(y, x)] == Tile.Wall:
            return False
        if self.objects[(y, x)] == Tile.Empty:
            return True
        if dir == Dirs.North:
            return self.can_move(y - 1, x, Dirs.North)
        if dir == Dirs.South:
            return self.can_move(y + 1, x, Dirs.South)
        if dir == Dirs.West:
            return self.can_move(y, x - 1, Dirs.West)
        if dir == Dirs.East:
            return self.can_move(y, x + 1, Dirs.East)

    def move_object(self, y, x, dir):
        if not self.can_move(y, x, dir):
            return False
        if self.objects[(y, x)] == Tile.Empty:
            return False
        if dir == Dirs.North:
            self.move_object(y - 1, x, Dirs.North)
            self.objects[(y - 1, x)] = self.objects[(y, x)]
            self.objects[(y, x)] = Tile.Empty
            return True
        if dir == Dirs.South:
            self.move_object(y + 1, x, Dirs.South)
            self.objects[(y + 1, x)] = self.objects[(y, x)]
            self.objects[(y, x)] = Tile.Empty
            return True
        if dir == Dirs.West:
            self.move_object(y, x - 1, Dirs.West)
            self.objects[(y, x - 1)] = self.objects[(y, x)]
            self.objects[(y, x)] = Tile.Empty
            return True
        if dir == Dirs.East:
            self.move_object(y, x + 1, Dirs.East)
            self.objects[(y, x + 1)] = self.objects[(y, x)]
            self.objects[(y, x)] = Tile.Empty
            return True

    def move(self, dir):
        assert self.objects[self.robot_pos] == Tile.Robot
        moved = self.move_object(*self.robot_pos, dir)
        if moved and dir == Dirs.North:
            self.robot_pos = (self.robot_pos[0] - 1, self.robot_pos[1])
        if moved and dir == Dirs.South:
            self.robot_pos = (self.robot_pos[0] + 1, self.robot_pos[1])
        if moved and dir == Dirs.West:
            self.robot_pos = (self.robot_pos[0], self.robot_pos[1] - 1)
        if moved and dir == Dirs.East:
            self.robot_pos = (self.robot_pos[0], self.robot_pos[1] + 1)

    def sum_gps(self):
        sum = 0
        for y in range(self.height):
            for x in range(self.width):
                if self.objects[(y, x)] == Tile.Box:
                    sum += 100 * y + x
        return sum

    @staticmethod
    def from_string(src):
        lines = src.split("\n")
        objects = {}
        height = len(lines)
        width = len(lines[0])
        robot_pos = None
        for y, line in enumerate(lines):
            for x, char in enumerate(line):
                tile = Tile.Empty
                if char == "#":
                    tile = tile.Wall
                elif char == "O":
                    tile = tile.Box
                elif char == "@":
                    robot_pos = (y, x)
                    tile = tile.Robot
                objects[(y, x)] = tile
        return Grid(objects, height, width, robot_pos)


def parse_moves(src):
    result = []
    for char in src:
        if char == "^":
            result.append(Dirs.North)
        elif char == "v":
            result.append(Dirs.South)
        elif char == "<":
            result.append(Dirs.West)
        elif char == ">":
            result.append(Dirs.East)
    return result


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)
    data = None
    with open(sys.argv[1], "rt") as f:
        data = f.read()
    grid_str, moves_str = data.split("\n\n")
    grid = Grid.from_string(grid_str)
    moves = parse_moves(moves_str)
    print(grid)
    for move in moves:
        grid.move(move)
    print(grid)
    print(f"GPS sum: {grid.sum_gps()}")
