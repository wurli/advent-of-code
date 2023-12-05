import re

with open("2023-Python/day-01/input.txt") as input_file:
    input = input_file.readlines()

def first_and_last_num(x: str):
    nums = re.findall(r"\d", x)
    out = nums[0] + nums[-1]
    return int(out)

# Part 1 ------------------------------------------------------
result1 = sum([first_and_last_num(line) for line in input])

# Part 2 ------------------------------------------------------
subs1 = {
    "one"  : "onee"  , "two"  : "twoo"  , "three": "threee",
    "four" : "fourr" , "five" : "fivee" , "six"  : "sixx",
    "seven": "sevenn", "eight": "eightt", "nine" : "ninee"
}

subs2 = {
    "one"  : 1, "two"  : 2, "three": 3, 
    "four" : 4, "five" : 5, "six"  : 6, 
    "seven": 7, "eight": 8, "nine" : 9,
}

def replace(x, replacements):
    for (pattern, replacement) in replacements.items():
        x = x.replace(pattern, replacement)
    return x

input2a = [replace(line, subs1) for line in input]
input2b = [replace(line, subs2) for line in input2a]
result2 = sum([first_and_last_num(line) for line in input2b])
