import numpy as np
import re

with open("2023-Python/day-03/input.txt") as input_file:
    input = input_file.read().split("\n")

input_parsed = np.array([[*line] for line in input])

def get_coords(pattern):
    coords = []
    for i, line in enumerate(input):
        for num in re.finditer(pattern, line):
            cols = list(range(num.start(), num.end()))
            rows = np.repeat(i, len(cols))
            coords.append([num.group(), np.column_stack((rows, cols))])
    return coords

def expand_coords(coords, shape):
    out = np.row_stack((
        coords + np.array([-1, 0]),
        coords + np.array([1, 0]),
        coords[0] + np.array([[-1, -1], [0, -1], [1, -1]]),
        coords[-1] + np.array([[-1, 1], [0, 1], [1, 1]])
    ))
    r, c = out[:, 0], out[:, 1]
    return out[(0 <= r) & (r < shape[0]) & (0 <= c) & (c < shape[1])]

def is_part_number(coords):
    surroundings = input_parsed[*expand_coords(coords, shape=input_parsed.shape).T]
    for char in surroundings.flatten():
        if not char in ".01234556789": 
            return True
    return False

# -- Part 1 -------------------------------------------------------------------------
num_coords = get_coords(r"\d+")
result1 = sum([int(x[0]) for x in num_coords if is_part_number(x[1])])

# -- Part 2 -------------------------------------------------------------------------
def has_overlap(a, b):
    a, b = np.unique(a, axis=0), np.unique(b, axis=0)
    return len(np.unique(np.row_stack((a, b)), axis=0)) < len(a) + len(b)

cog_coords = [expand_coords(coords[1], input_parsed.shape) for coords in get_coords(r"[*]")]

ratios = []
for cog in cog_coords:
    adjacent_nos = [int(n[0]) for n in num_coords if has_overlap(n[1], cog)]
    if len(adjacent_nos) == 2:
        ratios.append(np.prod(adjacent_nos)) 

result2 = sum(ratios)