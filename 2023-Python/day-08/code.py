import math
import re

with open("2023-Python/day-08/input.txt") as input_file:
    input = input_file.read()

instructions, maps = input.split("\n\n")

maps = [re.findall(r"[0-9A-Z]+", line) for line in maps.split("\n")]
maps = {map[0]: {"L": map[1], "R": map[2]} for map in maps}

def calculate_steps(position, test):
    steps = 0
    while not test(position):
        for instruction in instructions:
            position = maps[position][instruction]
            steps += 1
    return steps

# -- Part 1 --------------------------------------------------------------------
result1 = calculate_steps("AAA", lambda pos: pos == "ZZZ")

# -- Part 2 --------------------------------------------------------------------
steps_needed = [
    calculate_steps(pos, lambda pos: re.match(r".+Z$", pos)) 
    for pos in maps.keys()
    if re.match(r".+A$", pos)
]

result2 = math.lcm(*steps_needed)