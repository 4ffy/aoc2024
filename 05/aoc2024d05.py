#!/usr/bin/env python
import os
import sys
from collections import UserDict
from enum import Enum


class Color(Enum):
    White = 0
    Gray = 1
    Black = 2


class Node:
    def __init__(self, value):
        self.value = value
        self.neighbors = []
        self.color = Color.White

    def __eq__(self, other):
        return self.value == other.value

    def __str__(self):
        result = []
        result.append(f"{self.value}:")
        for value in self.neighbors:
            result.append(f"{value}")
        return " ".join(result)


class Graph(UserDict):

    def __str__(self):
        return "\n".join(str(x) for x in self.data.values())

    def add_pair(self, value_1, value_2):
        """Add a pair of values to the graph, where value_1 points to value_2.
        Create the nodes if they do not exist."""
        if value_1 not in self.data:
            self.data[value_1] = Node(value_1)
        if value_2 not in self.data:
            self.data[value_2] = Node(value_2)
        self.data[value_1].neighbors.append(value_2)

    def tsort(self):
        result = []
        start = None

        def has_unvisited_nodes():
            for node in self.data.values():
                if node.color == Color.White:
                    return True
            return False

        def visit(node):
            if node.color == Color.Black:
                return
            if node.color == Color.Gray:
                raise RuntimeError("Graph has a cycle.")
            node.color = Color.Gray
            for id in node.neighbors:
                visit(self.data[id])
            node.color = Color.Black
            result.append(node.value)

        for node in self.data.values():
            node.color = Color.White

        while has_unvisited_nodes():
            for node in self.data.values():
                if node.color == Color.White:
                    start = node
                    break
            visit(start)
        return result[::-1]


def parse_pair(pair_str):
    left, right = pair_str.split("|")
    return int(left), int(right)


def parse_pages(pages_str):
    return [int(x) for x in pages_str.split(",")]


def verify_pages(pages, rule):
    subrule = rule
    for x in pages:
        try:
            subrule = subrule[subrule.index(x):]
        except ValueError:
            return False
    return True


if __name__ == '__main__':
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
        print(graph)
        rule = graph.tsort()
        for pages_str in pages_section.split():
            pages = parse_pages(pages_str)
            if verify_pages(pages, rule):
                sum += pages[len(pages)//2]

    print(graph)
    print(f"Sum of middle: {sum}")
