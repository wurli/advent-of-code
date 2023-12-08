with open("2023-Python/day-07/input.txt") as input_file:
    input = [line.split() for line in input_file.readlines()]

# -- Part 1 --------------------------------------------------------------------
def hand_strength(hand):
    combo_strength = [sum([13**hand.count(card) for card in set(hand)])]
    card_strengths = ["23456789TJQKA".index(card) for card in hand]
    return combo_strength + card_strengths

input.sort(key = lambda x: hand_strength(x[0]))
result1 = sum([(pos + 1) * int(x[1]) for pos, x in enumerate(input)])

# -- Part 2 --------------------------------------------------------------------
def hand_strength2(hand):
    card_counts = [hand.count(card) for card in set(hand) if card != "J"] or [0]
    card_counts[card_counts.index(max(card_counts))] += hand.count("J")
    combo_strength = [sum([13**count for count in card_counts])] 
    card_strengths = ["J23456789TQKA".index(card) for card in hand]
    return combo_strength + card_strengths 

input.sort(key = lambda x: hand_strength2(x[0]))
result2 = sum([(pos + 1) * int(x[1]) for pos, x in enumerate(input)])
