from pathlib import Path
from typing import Tuple
import copy

dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]


def toMap(input: str) -> [[str]]:
    return list(map(lambda s: list(s), input.splitlines()))


def add(a: Tuple[int, int], b: Tuple[int, int]) -> Tuple[int, int]:
    return tuple(map(lambda i, j: i+j, a, b))


def findStart(map: [[str]]) -> Tuple[Tuple[int, int], int]:
    starts = ['^', '>', 'v', '<']
    for y, r in enumerate(map):
        for x, c in enumerate(r):
            if c in starts:
                return (y, x), starts.index(c)


def getSeen(inputMap: [[str]], start: (int, int), startDir: int):
    cur = start
    dir = startDir
    bounds = len(inputMap)
    seen = set()
    while True:
        if (cur, dir) in seen:
            return 'loop'
        seen.add((cur, dir))
        next = add(cur, dirs[dir])
        if next[0] >= bounds or next[1] >= bounds or next[0] < 0 or next[1] < 0:
            break
        if inputMap[next[0]][next[1]] == "#":
            dir = (dir + 1) % 4
            continue
        cur = next
    return set(map(lambda p: p[0], seen))


def part1(inputMap: [[str]], start: (int, int), startDir: int):
    return len(getSeen(inputMap, start, startDir))


def part2(inputMap: [[str]], start: (int, int), startDir: int):
    seen = getSeen(inputMap, start, startDir)
    count = 0
    for p in seen:
        map = copy.deepcopy(inputMap)
        map[p[0]][p[1]] = '#'
        if getSeen(map, start, startDir) == 'loop':
            count += 1
            inputMap[p[0]][p[1]] = 'O'
    return count


inputMap = toMap(Path('input.txt').read_text())
start, startDir = findStart(inputMap)
print(part1(inputMap, start, startDir))
print(part2(inputMap, start, startDir))
