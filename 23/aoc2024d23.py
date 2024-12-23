#!/usr/bin/env python
import itertools as it
import sys
import os


def parse_data(src):
    result = {}
    for line in src.split("\n"):
        left, right = line.split("-")
        left = int(left, 36)
        right = int(right, 36)
        if left not in result:
            result[left] = set()
        if right not in result:
            result[right] = set()
        result[left].add(right)
        result[right].add(left)
    return result


def three_neighbors(graph):
    result = set()
    for node in graph:
        neighbors = graph[node]
        if len(neighbors) >= 2:
            for n1, n2 in it.combinations(neighbors, 2):
                if n2 in graph[n1] and n1 in graph[n2]:
                    temp = [node, n1, n2]
                    temp.sort()
                    result.add(tuple(temp))
    return result


def starts_with_t(x):
    t = int("t", 36)
    digit = int("10", 36)
    if x // digit == t:
        return True
    return False


def count_t(triplets):
    sum = 0
    for a, b, c in triplets:
        if starts_with_t(a) or starts_with_t(b) or starts_with_t(c):
            sum += 1
    return sum


def largest_set(graph):
    cliques = []

    def bron_kerbosch(r, p, x):
        if len(p) == 0 and len(x) == 0:
            cliques.append(r)
        q = p.copy()
        for v in p:
            w = set([v])
            n = graph[v]
            bron_kerbosch(r | w, q & n, x & n)
            q -= w
            x |= w

    bron_kerbosch(set(), set(graph.keys()), set())
    return max(cliques, key=lambda x: len(x))


def back_to_string(id):
    chars = "0123456789abcdefghijklmnopqrstuvwxyz"
    divisor = int("10", 36)
    return chars[id // divisor] + chars[id % divisor]


def password(nodes):
    ids = list(back_to_string(x) for x in nodes)
    ids.sort()
    return ",".join(ids)


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
    graph = parse_data(data)
    print(f"Number of triplets: {count_t(three_neighbors(graph))}")

    largest = largest_set(graph)
    print(f"Password: {password(largest)}")
