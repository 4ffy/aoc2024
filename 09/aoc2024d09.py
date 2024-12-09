#!/usr/bin/env python
import sys
import os
from copy import deepcopy


class Block:
    def __init__(self, id, size, free):
        self.data = [id for x in range(size)] + [None for x in range(free)]
        self.size = size + free
        self.fill = size
        self.orig_fill = size

    def __str__(self):
        return str(self.data)

    def push(self, id):
        if self.fill < self.size:
            self.data[self.fill] = id
            self.fill += 1
            return
        raise RuntimeError("Pushed full")

    def pop(self):
        if self.fill > 0:
            self.fill -= 1
            ret = self.data[self.fill]
            self.data[self.fill] = None
            return ret
        raise RuntimeError("Popped empty")

    def pop_orig(self):
        if self.orig_fill > 0:
            self.orig_fill -= 1
            ret = self.data[self.orig_fill]
            self.data[self.orig_fill] = None
            return ret
        raise RuntimeError("Popped empty")

    def empty(self):
        return self.fill == 0

    def empty_orig(self):
        return self.orig_fill == 0

    def full(self):
        return self.fill == len(self.data)

    def fill_space(self):
        return self.fill

    def fill_orig(self):
        return self.orig_fill

    def free_space(self):
        return self.size - self.fill


def parse_blocks(src):
    if len(src) % 2 == 1:
        src += "0"
    result = [
        Block(i // 2, int(src[i]), int(src[i + 1]))
        for i in range(0, len(src) - 1, 2)
    ]
    return result


def compress(blocks):
    start = 0
    end = len(blocks) - 1
    while start < end:
        first = blocks[start]
        last = blocks[end]
        if first.full():
            start += 1
            continue
        if last.empty():
            end -= 1
            continue
        first.push(last.pop())


def defrag(blocks):
    for last in blocks[::-1]:
        if last.empty():
            continue
        start = 0
        first = blocks[start]
        while first != last:
            if first.free_space() >= last.fill_orig():
                while not last.empty_orig():
                    first.push(last.pop_orig())
                break
            else:
                start += 1
                first = blocks[start]


def checksum(blocks):
    sum = 0
    i = -1
    for block in blocks:
        for x in block.data:
            if x is None:
                x = 0
            i += 1
            sum += i * x
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
        data = f.read().rstrip()
    blocks = parse_blocks(data)
    blocks_2 = deepcopy(blocks)
    compress(blocks)
    defrag(blocks_2)
    print(f"Compress checksum: {checksum(blocks)}")
    print(f"Defrag checksum: {checksum(blocks_2)}")
