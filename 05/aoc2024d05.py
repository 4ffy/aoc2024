#!/usr/bin/env python
import itertools as it
import os
import sys
from collections import UserDict
from functools import cmp_to_key


class Node:
    def __init__(self, value):
        self.value = value
        self.neighbors = set()

    def __eq__(self, other):
        return self.value == other.value

    def __str__(self):
        result = []
        result.append(f"{self.value}:")
        for value in sorted(self.neighbors):
            result.append(f" {value}")
        return "".join(result)


class Graph(UserDict):
    def __str__(self):
        return "\n".join(
            str(x) for x in sorted(self.data.values(), key=lambda x: x.value)
        )

    def add_pair(self, value_1, value_2):
        """Add a pair of values to the graph, where value_1 points to value_2.
        Create the nodes if they do not exist."""
        if value_1 not in self.data:
            self.data[value_1] = Node(value_1)
        if value_2 not in self.data:
            self.data[value_2] = Node(value_2)
        self.data[value_1].neighbors.add(value_2)

    def has_neighbor(self, value_1, value_2):
        return value_2 in self.data[value_1].neighbors

    def compare_nodes(self, value_1, value_2):
        # A node is "greater" than another if it has no connection.
        if value_2 in self.data[value_1].neighbors:
            return -1
        return 1


def parse_pair(pair_str):
    left, right = pair_str.split("|")
    return int(left), int(right)


def parse_pages(pages_str):
    return [int(x) for x in pages_str.split(",")]


def verify_pages(graph, pages):
    for page_1, page_2 in it.combinations(pages, 2):
        if not graph.has_neighbor(page_1, page_2):
            return False
    return True


def fix_invalid(graph, pages):
    return list(sorted(pages, key=cmp_to_key(graph.compare_nodes)))


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)

    graph = Graph()
    sum = 0
    fixed_sum = 0
    with open(sys.argv[1], "rt") as f:
        data = f.read()
        order_section, pages_section = data.split("\n\n")
        for pair_str in order_section.split():
            graph.add_pair(*parse_pair(pair_str))
        for i, pages_str in enumerate(pages_section.split()):
            pages = parse_pages(pages_str)
            valid = verify_pages(graph, pages)
            if valid:
                sum += pages[len(pages) // 2]
            else:
                fixed = fix_invalid(graph, pages)
                fixed_sum += fixed[len(fixed) // 2]
        print(f"Sum of valid: {sum}")
        print(f"Sum of fixed: {fixed_sum}")
