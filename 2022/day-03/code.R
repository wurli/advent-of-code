library(tidyverse)

input <- read_lines("2022/day-03/input.txt")

priorities <- input |> 
  strsplit("") |> 
  map(match, c(letters, LETTERS))

# Part 1
priorities |> 
  map_dbl(~ intersect(
    .x[seq_along(.x) <= length(.x) / 2],
    .x[seq_along(.x) >  length(.x) / 2]
  )) |> 
  sum()

# Part 2
priorities |> 
  tapply(
    (seq_along(input) - 1) %/% 3,
    function(x) reduce(x, intersect)
  ) |> 
  sum()
