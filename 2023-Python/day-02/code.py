import re

with open("2023-Python/day-02/input.txt") as input_file:
    input = input_file.readlines()

# -- Parsing -----------------------------------------------------------------------
def parse_line(line):
    line = line.split(": ")
    return {
        "ID": int(re.findall(r"\d+", line[0])[0]),
        "Sets": [parse_set(set) for set in line[1].split("; ")]
    }

def parse_set(set):
    return {
        re.findall(r"(red|green|blue)", turn)[0]: int(re.findall(r"\d+", turn)[0])
        for turn in set.split(", ")
    }
    
input_parsed = [parse_line(line) for line in input]

# -- Part 1 -----------------------------------------------------------------------
def is_possible(sets, reds, greens, blues):
    for set in sets:
        if set.get("red"  , 0) > reds  : return False
        if set.get("green", 0) > greens: return False
        if set.get("blue" , 0) > blues : return False
    return True

result1 = sum([
    game["ID"] 
    for game in input_parsed 
    if is_possible(game["Sets"], reds=12, greens=13, blues=14)
])

# -- Part 2 -----------------------------------------------------------------------
def min_set_power(sets):
    min_set = {"red": 0, "green": 0, "blue": 0}
    for set in sets:
        for colour, n in set.items():
            min_set.update({colour: max(min_set[colour], n)})
    return min_set["red"] * min_set["green"] * min_set["blue"]

result2 = sum([min_set_power(game["Sets"]) for game in input_parsed])
