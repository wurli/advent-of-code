import re
import math
import numpy as np

with open("2023-Python/day-04/input.txt") as input_file:
    input = [re.sub(r"\s+", " ", line) for line in input_file.readlines()]

input_parsed = [
    {
        "Card_No": int(re.findall(r"(?<=Card )\d+", line)[0]),
        "Winning": set(map(int, re.split(r"\s+", re.findall(r"(?<=: ).+(?= \|)", line)[0]))),
        "Present": set(map(int, re.split(r"\s+", re.findall(r"(?<=\| ).+(?= )", line)[0])))
    }
    for line in input
]

# -- Part 1 ------------------------------------------------------------------------------------
n_matches = [len(card["Winning"].intersection(card["Present"])) for card in input_parsed]
scores = [math.floor(2 ** (n - 1)) for n in n_matches]
result1 = sum(scores)

# -- Part 2 -------------------------------------------------------------------------------------
n_scratchcards = np.repeat(1, len(input_parsed))
for card, matches in enumerate(n_matches):
    n_scratchcards[range(card + 1, card + 1 + matches)] += n_scratchcards[card] 

result2 = sum(n_scratchcards)
