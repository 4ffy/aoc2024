#!/usr/bin/env python
import itertools as it
import os
import sys
from collections import UserDict


class Node:
    def __init__(self, value):
        self.value = value
        self.neighbors = []
        self.visited = False

    def __eq__(self, other):
        return self.value == other.value

    def __str__(self):
        result = []
        result.append(f"{self.value}")
        if self.visited:
            result.append("(v)")
        else:
            result.append("( )")
        result.append(":")
        for value in self.neighbors:
            result.append(f" {value}")
        return "".join(result)


class Graph(UserDict):
    def __str__(self):
        return "\n".join(str(x) for x in sorted(self.data.values(), key=lambda x: x.value))

    def add_pair(self, value_1, value_2):
        """Add a pair of values to the graph, where value_1 points to value_2.
        Create the nodes if they do not exist."""
        if value_1 not in self.data:
            self.data[value_1] = Node(value_1)
        if value_2 not in self.data:
            self.data[value_2] = Node(value_2)
        self.data[value_1].neighbors.append(value_2)

    def is_reachable(self, src, dest):
        """Walk the graph to determine whether value_2 is reachable from
        value_1."""

        def walk(node: Node) -> bool:
            if node.value == dest:
                return True
            node.visited = True
            for id in node.neighbors:
                if not self.data[id].visited:
                    if walk(self.data[id]):
                        return True
            return False

        for node in self.data.values():
            node.visited = False
        return walk(self.data[src])


def parse_pair(pair_str):
    left, right = pair_str.split("|")
    return int(left), int(right)


def parse_pages(pages_str):
    return [int(x) for x in pages_str.split(",")]


def verify_pages(graph, pages):
    for page_1, page_2 in it.pairwise(pages):
        print(page_1, page_2)
        if graph.is_reachable(page_2, page_1):
            return False
    return True


if __name__ == "__main__":
    sys.argv = ["penis", "input"]
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)

    graph = Graph()
    sum = 0
    with open(sys.argv[1], "rt") as f:
        data = f.read()
        order_section, pages_section = data.split("\n\n")
        for pair_str in order_section.split():
            graph.add_pair(*parse_pair(pair_str))
        for i, pages_str in enumerate(pages_section.split()):
            if i != 3:
                continue
            pages = parse_pages(pages_str)
            valid = verify_pages(graph, pages)
            print(f"{valid}: {pages}")
            if valid:
                sum += pages[len(pages) // 2]
        print(graph)
        print(f"Sum of center: {sum}")
