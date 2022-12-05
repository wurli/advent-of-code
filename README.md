Solutions to Advent of Code 2022
================
Jacob Scott

These are my solutions to [Advent of Code](https://adventofcode.com/)
2022 🎄🎄🎄

## day-05

``` r
crates_raw       <- readLines("2022/day-05/crates.txt")
instructions_raw <- readLines("2022/day-05/instructions.txt")

crates <- crates_raw |> 
  strsplit("") |> 
  vapply(\(x) x[((0:8) * 4) + 2], character(9)) |> 
  lapply(y = _, X = 1:9, FUN = \(x, y) y[x,][y[x,] != " "])

instructions <- instructions_raw |> 
  regmatches(gregexpr("\\d+", instructions_raw)) |> 
  lapply(as.numeric) 

rearrange <- function(crates, instructions, reverse = TRUE) {
  reverse <- if (reverse) rev else identity
  
  for (ins in instructions) {
    crates[[ins[3]]] <- c(
      reverse(head(crates[[ins[2]]], ins[1])),
      crates[[ins[3]]]
    )
    crates[[ins[2]]] <- tail(crates[[ins[2]]], -ins[1])
  }
  
  crates 
}

# Part 1
crates |> 
  rearrange(instructions, reverse = TRUE) |> 
  vapply(head, character(1), 1) |> 
  paste(collapse = "")
#>  [1] "FWSHSPJWM"

# Part 2
crates |> 
  rearrange(instructions, reverse = FALSE) |> 
  vapply(head, character(1), 1) |> 
  paste(collapse = "")
#>  [1] "PWPWHGFZS"
```

## day-04

``` r
input <- readLines("2022/day-04/input.txt") |> 
  gsub("-", ",", x = _) |> 
  read.csv(text = _, col.names = c("x1", "x2", "y1", "y2"))

# Part 1
input |> 
  subset((x1 <= y1 & y2 <= x2) | (y1 <= x1 & x2 <= y2)) |> 
  nrow()
#>  [1] 549

# Part 2
input |> 
  subset((x1 <= y1 & y1 <= x2) | (y1 <= x1 & x1 <= y2)) |> 
  nrow()
#>  [1] 930
```

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
    reduce, intersect
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
