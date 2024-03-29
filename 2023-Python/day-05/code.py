import functools as fn
import re

# -- Importing/parsing ---------------------------------------------------------
with open("2023-Python/day-05/input.txt") as input_file:
    input = input_file.read().split("\n\n")

seeds = [int(x) for x in re.findall(r"\d+", input[0])]

maps = [
    [[int(x) for x in map.split()] for map in block.split("\n")[1:]]
    for block in input[1:]
]

# -- Helper functions ----------------------------------------------------------
def range_intersect(x, y):
    out = [range(max(x.start, y.start), min(x.stop, y.stop))]
    return [r for r in out if len(r) > 0]

def range_difference(x, y): 
    x0, x1, y0, y1 = x.start, x.stop, y.start, y.stop
    out = [range(x0, min(x1, y0)), range(max(y1, x0), x1)]
    return [r for r in out if len(r) > 0]

def range_shift(x, y):
    if len(x) == 0: return x
    return [range(r.start + y, r.stop + y, r.step) for r in x]

def remap(ranges, map):
    new_ranges = []
    for m in map:
        overlap = range(m[1], m[1] + m[2])
        moved_parts, static_parts = [], []
        for r in ranges + static_parts:
            moved_parts += range_shift(range_intersect(r, overlap), m[0] - m[1])
            static_parts += range_difference(r, overlap)
        new_ranges += moved_parts
        ranges = static_parts
    return new_ranges + static_parts

# -- Part 1 --------------------------------------------------------------------
seed_ranges = [range(x, x + 1) for x in seeds]
result1 = min(r.start for r in fn.reduce(remap, maps, seed_ranges))

# -- Part 2 --------------------------------------------------------------------
seed_ranges2 = [
    range(seeds[i], seeds[i] + seeds[i + 1]) 
    for i in range(0, len(seeds), 2)
]

result2 = min(r.start for r in fn.reduce(remap, maps, seed_ranges2))
