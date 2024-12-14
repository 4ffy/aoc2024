#!/usr/bin/env python
import sys
import os


class Robot:
    def __init__(self, x, y, dx, dy):
        self.x = x
        self.y = y
        self.dx = dx
        self.dy = dy

    def move(self, width, height, step=1):
        self.x = (self.x + self.dx * step) % width
        self.y = (self.y + self.dy * step) % height


def parse_robot(data):
    position, velocity = data.split(' ')
    x, y = [int(x) for x in position.split("=")[1].split(",")]
    dx, dy = [int(x) for x in velocity.split("=")[1].split(",")]
    return Robot(x, y, dx, dy)


def prod_quadrants(robots, width, height):
    tophalf = lambda r: 0 <= r.y < height // 2
    bottomhalf = lambda r: height // 2 < r.y < height
    lefthalf = lambda r: 0 <= r.x < width // 2
    righthalf = lambda r: width // 2 < r.x < width
    q1 = [r for r in robots if tophalf(r) and lefthalf(r)]
    q2 = [r for r in robots if tophalf(r) and righthalf(r)]
    q3 = [r for r in robots if bottomhalf(r) and righthalf(r)]
    q4 = [r for r in robots if bottomhalf(r) and lefthalf(r)]
    return len(q1) * len(q2) * len(q3) * len(q4)


def print_grid(robots, width, height):
    for y in range(height):
        this_line = set(r.x for r in robots if r.y == y)
        for x in range(width):
            if x in this_line:
                print("#", end="")
            else:
                print(" ", end="")
        print()


def overlap(robots):
    overlap = 0
    db = set()
    for robot in robots:
        if (robot.x, robot.y) in db:
            overlap += 1
        else:
            db.add((robot.x, robot.y))
    return overlap


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
    robots = [parse_robot(x) for x in data.split("\n")]
    for i in range(15000):
        if i == 100:
            print(f"Product of quadrants: {prod_quadrants(robots, 101, 103)}")
        if overlap(robots) == 0:
            print(f"Christmas detected: {i}")
        for robot in robots:
            robot.move(101, 103, 1)
