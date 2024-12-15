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
    WideLeft = 4
    WideRight = 5


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
                elif tile == Tile.WideLeft:
                    result.append("[")
                elif tile == Tile.WideRight:
                    result.append("]")
                else:
                    result.append("R")
            result.append("\n")
        return "".join(result)

    def can_move(self, y, x, dir):
        tile = self.objects[(y, x)]
        if tile is None or tile == Tile.Wall:
            return False
        if tile == Tile.Empty:
            return True
        if dir == Dirs.North:
            if tile == Tile.WideLeft:
                return self.can_move(y - 1, x, Dirs.North) and self.can_move(
                    y - 1, x + 1, Dirs.North
                )
            elif tile == Tile.WideRight:
                return self.can_move(y - 1, x, Dirs.North) and self.can_move(
                    y - 1, x - 1, Dirs.North
                )
            else:
                return self.can_move(y - 1, x, Dirs.North)

        if dir == Dirs.South:
            if tile == Tile.WideLeft:
                return self.can_move(y + 1, x, Dirs.South) and self.can_move(
                    y + 1, x + 1, Dirs.South
                )
            elif tile == Tile.WideRight:
                return self.can_move(y + 1, x, Dirs.South) and self.can_move(
                    y + 1, x - 1, Dirs.South
                )
            else:
                return self.can_move(y + 1, x, Dirs.South)

        if dir == Dirs.West:
            return self.can_move(y, x - 1, Dirs.West)

        if dir == Dirs.East:
            return self.can_move(y, x + 1, Dirs.East)

    def move_object(self, y, x, dir):
        tile = self.objects[(y, x)]
        if not self.can_move(y, x, dir):
            return False
        if tile == Tile.Empty:
            return False
        if dir == Dirs.North:
            if tile == Tile.WideLeft:
                self.move_object(y - 1, x, Dirs.North)
                self.move_object(y - 1, x + 1, Dirs.North)
                self.objects[(y - 1, x)] = Tile.WideLeft
                self.objects[(y - 1, x + 1)] = Tile.WideRight
                self.objects[(y, x)] = Tile.Empty
                self.objects[(y, x + 1)] = Tile.Empty
            elif tile == Tile.WideRight:
                self.move_object(y - 1, x, Dirs.North)
                self.move_object(y - 1, x - 1, Dirs.North)
                self.objects[(y - 1, x)] = Tile.WideRight
                self.objects[(y - 1, x - 1)] = Tile.WideLeft
                self.objects[(y, x)] = Tile.Empty
                self.objects[(y, x - 1)] = Tile.Empty
            else:
                self.move_object(y - 1, x, Dirs.North)
                self.objects[(y - 1, x)] = tile
                self.objects[(y, x)] = Tile.Empty
            return True
        if dir == Dirs.South:
            if tile == Tile.WideLeft:
                self.move_object(y + 1, x, Dirs.South)
                self.move_object(y + 1, x + 1, Dirs.South)
                self.objects[(y + 1, x)] = Tile.WideLeft
                self.objects[(y + 1, x + 1)] = Tile.WideRight
                self.objects[(y, x)] = Tile.Empty
                self.objects[(y, x + 1)] = Tile.Empty
            elif tile == Tile.WideRight:
                self.move_object(y + 1, x, Dirs.South)
                self.move_object(y + 1, x - 1, Dirs.South)
                self.objects[(y + 1, x)] = Tile.WideRight
                self.objects[(y + 1, x - 1)] = Tile.WideLeft
                self.objects[(y, x)] = Tile.Empty
                self.objects[(y, x - 1)] = Tile.Empty
            else:
                self.move_object(y + 1, x, Dirs.South)
                self.objects[(y + 1, x)] = tile
                self.objects[(y, x)] = Tile.Empty
            return True
        if dir == Dirs.West:
            self.move_object(y, x - 1, Dirs.West)
            self.objects[(y, x - 1)] = tile
            self.objects[(y, x)] = Tile.Empty
            return True
        if dir == Dirs.East:
            self.move_object(y, x + 1, Dirs.East)
            self.objects[(y, x + 1)] = tile
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
                tile = self.objects[(y, x)]
                if tile == Tile.Box or tile == Tile.WideLeft:
                    sum += 100 * y + x
        return sum

    def widen(self):
        self.width = grid.width * 2
        self.robot_pos = (self.robot_pos[0], self.robot_pos[1] * 2)
        objects = {}
        for (y, x), tile in grid.objects.items():
            if tile == Tile.Box:
                objects[(y, 2 * x)] = Tile.WideLeft
                objects[(y, 2 * x + 1)] = Tile.WideRight
            elif tile == Tile.Robot:
                objects[(y, 2 * x)] = Tile.Robot
                objects[(y, 2 * x + 1)] = Tile.Empty
            else:
                objects[(y, 2 * x)] = tile
                objects[(y, 2 * x + 1)] = tile
        self.objects = objects

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
    wide = Grid.from_string(grid_str)
    wide.widen()
    moves = parse_moves(moves_str)
    for move in moves:
        grid.move(move)
        wide.move(move)
    print(f"Narrow GPS sum: {grid.sum_gps()}")
    print(f"Wide GPS sum: {wide.sum_gps()}")
