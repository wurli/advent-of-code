import functools as fn
import re

with open("2023-Python/day-05/input.txt") as input_file:
    input = input_file.read().split("\n\n")

seeds = [int(x) for x in re.findall(r"\d+", input[0])]

maps = [
    [[int(x) for x in map.split()] for map in block.split("\n")[1:]]
    for block in input[1:]
]

def range_intersect(x, y):
    out = [range(max(x.start, y.start), min(x.stop, y.stop))]
    return [r for r in out if len(r) > 0]
# range_intersect(range(5, 10), range(7, 15))
# range_intersect(range(5, 10), range(10, 15))
# range_intersect(range(5, 10), range(-3, 2))
# range_intersect(range(5, 10), range(1, 15))

def range_difference(x, y): 
    x0, x1, y0, y1 = x.start, x.stop, y.start, y.stop
    out = [range(x0, min(x1, y0)), range(max(y1, x0), x1)]
    return [r for r in out if len(r) > 0]
# range_difference(range(5, 10), range(7, 15))
# range_difference(range(5, 10), range(10, 15))
# range_difference(range(5, 10), range(-3, 2))
# range_difference(range(1, 10), range(4, 8))
# range_difference(range(1, 10), range(10, 1))

def range_shift(x, y):
    if len(x) == 0: return x
    return [range(r.start + y, r.stop + y, r.step) for r in x]
# range_shift(range(1, 3), 3)
# range_shift(range(1, 3), -2)

def remap(ranges, map):
    ranges = ranges.copy()
    new_ranges = []
    for line in map:
        bit_to_move = range(line[1], line[1] + line[2])
        moved_parts, static_parts = [], []
        for r in ranges + static_parts:
            moved_parts += range_shift(range_intersect(r, bit_to_move), line[0] - line[1])
            static_parts += range_difference(r, bit_to_move)
        new_ranges += moved_parts
        ranges = static_parts
    return new_ranges + static_parts

# -- Part 1 --------------------------------------------------------------
seed_ranges = [range(x, x + 1) for x in seeds]
min(min(r) for r in fn.reduce(remap, maps, seed_ranges))
# 600279879

# -- Part 2 --------------------------------------------------------------
seed_ranges2 = [
    range(seeds[i], seeds[i] + seeds[i + 1]) 
    for i in range(0, len(seeds), 2)
]

min(min(r) for r in fn.reduce(remap, maps, seed_ranges2))
# 20191102