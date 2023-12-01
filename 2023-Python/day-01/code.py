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
nums = {
    "one"  : 1, "two"  : 2, "three": 3, 
    "four" : 4, "five" : 5, "six"  : 6, 
    "seven": 7, "eight": 8, "nine" : 9,
    "twone": 21,
    "sevenine": 79,
    "eightwo": 82,
    "eightwone": 821
}

def multi_replace(x, replacements):
    pattern = "|".join(nums.keys())
    def switch(match):
        return str(replacements[match[0]])
    return re.sub(pattern, switch, x)

input2 = [multi_replace(line, nums) for line in input]
result2 = sum([first_and_last_num(line) for line in input2])

# 54807 - too low
