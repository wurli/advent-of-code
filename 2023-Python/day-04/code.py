import numpy as np
import math
import re

with open("2023-Python/day-04/input.txt") as input_file:
    input = input_file.readlines()

def parse_line(line):
    a, b, c = re.split(r"[|:]", line)
    return set(map(int, b.split())), set(map(int, c.split()))

cards = [parse_line(line) for line in input]

# -- Part 1 ---------------------------------------------------------------------------
n_matches = [len(card[0] & card[1]) for card in cards]
result1 = sum([math.floor(2 ** (n - 1)) for n in n_matches])

# -- Part 2 ---------------------------------------------------------------------------
n_cards = np.repeat(1, len(cards))
for card, matches in enumerate(n_matches):
    n_cards[range(card + 1, card + 1 + matches)] += n_cards[card] 

result2 = sum(n_cards)
