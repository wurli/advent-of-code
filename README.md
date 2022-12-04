Solutions to Advent of Code 2022
================
Jacob Scott

These are my solutions to [Advent of Code](https://adventofcode.com/)
2022 🎄🎄🎄

## day-03

``` r
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
#>  [1] 8394

# Part 2
priorities |> 
  tapply(
    (seq_along(input) - 1) %/% 3,
    function(x) reduce(x, intersect)
  ) |> 
  sum()
#>  [1] 2413
```

## day-02

``` r
library(tidyverse)

strategy <- as_tibble(read.table(
  "2022/day-02/input.txt",
  col.names = c("x", "y")
))

# Part 1
strategy |> 
  mutate(
    across(
      c(x, y), 
      ~ case_when(
        . %in% c("A", "X") ~ 1,
        . %in% c("B", "Y") ~ 2,
        . %in% c("C", "Z") ~ 3
      )
    ),
    result = (y - x) %% 3,
    result = case_when(
      result == 1 ~ 6,
      result == 2 ~ 0,
      result == 0 ~ 3
    ),
    score = y + result
  ) |> 
  summarise(
    score = sum(score)
  )
#>  # A tibble: 1 × 1
#>    score
#>    <dbl>
#>  1 11063

# Part 2
strategy |> 
  mutate(
    x = case_when(
      x %in% c("A", "X") ~ 1,
      x %in% c("B", "Y") ~ 2,
      x %in% c("C", "Z") ~ 3
    ),
    result = case_when(
      y == "X" ~ 2, # loss
      y == "Y" ~ 0, # draw
      y == "Z" ~ 1  # win
    ),
    y = (x + result - 1) %% 3 + 1,
    result = case_when(
      result == 1 ~ 6,
      result == 2 ~ 0,
      result == 0 ~ 3
    ),
    score = y + result
  ) |> 
  summarise(
    score = sum(score)
  )
#>  # A tibble: 1 × 1
#>    score
#>    <dbl>
#>  1 10349
```

## day-01

``` r
library(tidyverse)

input <- readr::read_file("2022/day-01/input.txt") 

total_calories <- input |> 
  strsplit("\n\n") |> 
  pluck(1) |> 
  map_dbl(\(x) {
    
    x <- strsplit(x, "\n")[[1]]
    
    x |> 
      as.numeric() |> 
      sum()
    
  }) |> 
  sort(decreasing = TRUE) 

# Part 1
total_calories[1]
#>  [1] 71023

# Part 2
sum(total_calories[1:3])
#>  [1] 206289
```
