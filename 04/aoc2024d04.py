#!/usr/bin/env python
import numpy as np
import os
import re
import sys


def columns(arr):
    return ("".join(arr[:, x]) for x in range(arr.shape[1]))


def columns_rev(arr):
    return ("".join(arr[:, x])[::-1] for x in range(arr.shape[1]))


def rows(arr):
    return ("".join(arr[y]) for y in range(arr.shape[0]))


def rows_rev(arr):
    return ("".join(arr[y])[::-1] for y in range(arr.shape[0]))


def diag_1(arr):
    diag_count = sum(arr.shape)
    return ("".join(arr.diagonal(d)) for d in range(1 - diag_count, diag_count))


def diag_1_rev(arr):
    diag_count = sum(arr.shape)
    return (
        "".join(arr.diagonal(d))[::-1]
        for d in range(1 - diag_count, diag_count)
    )


def diag_2(arr):
    diag_count = sum(arr.shape)
    return (
        "".join(np.fliplr(arr).diagonal(d))
        for d in range(1 - diag_count, diag_count)
    )


def diag_2_rev(arr):
    diag_count = sum(arr.shape)
    return (
        "".join(np.fliplr(arr).diagonal(d))[::-1]
        for d in range(1 - diag_count, diag_count)
    )


def count_xmas(it):
    count = 0
    for line in it:
        for match in re.finditer(r"XMAS", line):
            count += 1
    return count


def count_all(arr):
    sum = 0
    for it in [
        rows,
        columns,
        diag_1,
        diag_2,
        rows_rev,
        columns_rev,
        diag_1_rev,
        diag_2_rev,
    ]:
        sum += count_xmas(it(arr))
    return sum


def windows(arr):
    return (
        arr[y : y + 3, x : x + 3]
        for y in range(arr.shape[0] - 2)
        for x in range(arr.shape[1] - 2)
    )


def check_window(win):
    if win[1, 1] != "A":
        return False
    diag_1_mas = (win[0, 0] == "M" and win[2, 2] == "S") or (
        win[0, 0] == "S" and win[2, 2] == "M"
    )
    diag_2_mas = (win[0, 2] == "M" and win[2, 0] == "S") or (
        win[0, 2] == "S" and win[2, 0] == "M"
    )
    if diag_1_mas and diag_2_mas:
        return True
    return False


def count_crosses(arr):
    count = 0
    for win in windows(arr):
        if check_window(win):
            count += 1
    return count


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} input", file=sys.stderr)
        exit(1)
    if not os.path.exists(sys.argv[1]):
        print(f"File not found: '{sys.argv[1]}'", file=sys.stderr)
        exit(1)
    with open(sys.argv[1], "rt") as f:
        grid = np.array([list(line) for line in f.read().rstrip().split()])
        print(f"XMAS count: {count_all(grid)}")
        print(f"X-MAS count: {count_crosses(grid)}")
