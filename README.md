Solutions to Advent of Code 2022
================
Jacob Scott

These are my solutions to [Advent of Code](https://adventofcode.com/)
2022 🎄🎄🎄

## day-08

``` r
input <- readLines("2022/day-08/input.txt") |> 
  strsplit("") |>
  sapply(as.numeric, USE.NAMES = FALSE) 

rotate_each <- function(x, clockwise = TRUE) {
  lapply(seq_along(x), \(i) Reduce(
    \(a, b) if (clockwise) t(apply(a, 2, rev)) else apply(t(a), 2, rev),
    seq_len(i - 1), x[[i]]
  ))
}

# Part 1
rep(list(input), 4) |> 
  rotate_each(clockwise = TRUE) |> 
  lapply(apply, 2, \(x) sapply(seq_along(x), \(i) {
    i == 1 || max(head(x, i - 1)) < x[i]
  })) |> 
  rotate_each(clockwise = FALSE) |> 
  Reduce(x = _, f = `|`, init = FALSE) |> 
  sum()
#>  [1] 1803

# Part 2
rep(list(input), 4) |> 
  rotate_each(clockwise = TRUE) |> 
  lapply(apply, 2, \(x) sapply(seq_along(x), \(i) {
    unblocked <- x[rev(seq_len(i - 1))] < x[i]
    sum(cumprod(unblocked)) + !all(unblocked)
  })) |> 
  rotate_each(clockwise = FALSE) |> 
  Reduce(x = _, f = `*`, init = 1) |> 
  max()
#>  [1] 268912
```

## day-07

``` r
input <- readLines("2022/day-07/input.txt")

# 1. Parse console log to get a representation of the filesystem
fs <- Reduce(x = input, init = list(fs = list(), pos = NULL), \(x, y) {
  
  if (grepl("^\\$ cd [^.]"  , y)) x$pos <- c(x$pos, sub("^\\$ cd ", "", y))
  if (grepl("^\\$ cd \\.\\.", y)) x$pos <- head(x$pos, -1)
  
  x$fs[[x$pos]] <- c(x$fs[[x$pos]], if (!grepl("^\\$", y)) setNames(
    list(if (!grepl("^dir", y)) as.integer(gsub("\\D", "", y))), 
    sub("^.+ ", "", y)
  ))
  
  x
  
})$fs

# 2. Get size of each directory
get_sizes <- function(x) Reduce(
  \(i, j) c(i, setNames(sum(unlist(x[[j]])), j), get_sizes(x[[j]])), 
  names(Filter(is.list, x)), NULL
)

sizes <- get_sizes(fs)

# Part 1
sum(sizes[sizes <= 100000])
#>  [1] 1749646

# Part 2
min(sizes[sizes >= 30000000 - (70000000 - max(sizes))])
#>  [1] 1498966
```

## day-06

``` r
input <- strsplit(readLines("2022/day-06/input.txt"), "")[[1]]

first_marker <- function(x, len = 4) {
  len - 1 + match(TRUE, sapply(
    seq_len(length(x) - len),
    \(i) !any(duplicated(x[seq_len(len) + i - 1]))
  ))
}

# Part 1
first_marker(input, len = 4)
#>  [1] 1987

# Part 2
first_marker(input, len = 14)
#>  [1] 3059
```

## day-05

``` r
crates_raw       <- read.fwf("2022/day-05/crates.txt", rep(1, 9 * 4 - 1))
instructions_raw <- readLines("2022/day-05/instructions.txt")

crates <- crates_raw[((0:8) * 4) + 2] |> 
  lapply(\(x) x[x != " "])

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
