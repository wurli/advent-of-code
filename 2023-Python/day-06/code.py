# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The speed of the boat can be viewed a linear function of time spent charging:
#
#    (A)  speed = time_spent_charging
#
# *  time_spent_charging = 0   =>   speed = 0 
# *  time_spent_charging = 7   =>   speed = 7
#
# The time spent travelling can be expressed as a function of time spent 
# charging *and* the race length:
#
#    (B)  time_spent_travelling = race_length - time_spent_charging
#
# *  time_spent_charging = 0, race_length = 7   =>   time_spent_travelling = 7
# *  time_spent_charging = 7, race_length = 7   =>   time_spent_travelling = 0
#
# The total distance travelled can then be calculated as:
#
#    (C)  distance = speed * time_spent_travelling
#             = time_spent_charging * (race_length - time_spent_charging)
#
# With some algebra, we then get:
#
#    (D)  0 = time_spent_charging^2 - race_length*time_spent_charging + distance
#
# In each case, we know the total distance we need to travel in order to win.
# Substituting this in for [distance], we get a quadratic equation. We can
# solve this (using the quadratic formula) to get two values for 
# [time_spent_charging] which will get the boat exactly the distance needed.
# Since the equation has a positive coefficient for time_spent_charging^2, 
# all intermediate values will also win.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
import math

with open("2023-Python/day-06/input.txt") as input_file:
    input = input_file.readlines()
    
def quadratic_roots(a, b, c):
    d = math.sqrt(b**2 - (4 * a * c))
    return [(-b - d) / (2 * a), (-b + d) / (2 * a)]

def n_winning(time, dist):
    lower, upper = quadratic_roots(1, -time, dist + 0.000001)
    return math.floor(upper) - math.ceil(lower) + 1    

# -- Part 1 --------------------------------------------------------------------
times, dists = [[int(x) for x in line.split()[1:]] for line in input]
result1 = math.prod([n_winning(x[0], x[1]) for x in zip(times, dists)])

# -- Part 2 --------------------------------------------------------------------
result2 = n_winning(*[int("".join(line.split()[1:])) for line in input])
