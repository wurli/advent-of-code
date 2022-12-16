Solutions to Advent of Code 2022
================
Jacob Scott

These are my solutions to [Advent of Code](https://adventofcode.com/)
2022 🎄🎄🎄

## day-15

``` r
input <- readLines("2022/day-15/input.txt") |> 
  sapply(\(x) as.integer(strsplit(x, "[^0-9-]+")[[1]][-1]), USE.NAMES = FALSE) 

distances <- abs(input[1,] - input[3,]) + abs(input[2,] - input[4,])

# Part 1 -----------------------------------------------------------------------

# a. Get range covered by each sensor
ranges <- seq_len(ncol(input)) |> 
  lapply(\(i) {
    h_len <- distances[i] - abs(input[2,][i] - 2000000)
    if (h_len < 0) NULL else list(input[1,][i] + h_len * c(-1, 1))
  }) |> 
  Filter(x = _, \(x) !is.null(x))

# b. Repeatedly merge ranges down so `diff()` can be used to get total coverage
ranges <- Reduce(x = ranges[order(sapply(ranges, \(x) x[[1]][1]))], \(i, j) {
  x <- i[[length(i)]]
  y <- j[[1]]
  if (y[1] - 1 <= x[2]) c(i[-length(i)], list(range(c(x, y)))) else c(i, j)
})

# Total = covered spaces - known beacons
sum(sapply(ranges, diff) + 1) - ncol(unique(input[3:4,][,input[4,] == 2000000], MARGIN = 2)) 
#>  [1] 4737443

# Part 2 -----------------------------------------------------------------------

# Check if a point is in range of any beacon
in_range <- function(p) {
  for (i in seq_len(ncol(input))) {
    if (abs(p[1] - input[,i][1]) + abs(p[2] - input[,i][2]) <= distances[i]) return(TRUE)
  }
  FALSE
}

# Rotate points by 45 degrees in either direction
rotate45 <- function(mat, clockwise = TRUE) {
  i <- if (clockwise) pi / 4 else -pi / 4
  matrix(c(cos(i), sin(i), -sin(i), cos(i)), ncol = 2) %*% mat
}

# Points will only be 'gaps' if they lie along the edges of covered regions.
# So, just test points where such lines intersect. Intersections can be obtained
# by rotating by 45 degrees, then getting all combinations of x and y coordinates.
maybe_gaps <- seq_len(ncol(input)) |> 
  lapply(\(i) lapply(list(c(1, 0), c(-1, 0)), \(x) input[,i][1:2] + x * (distances[i] + 1))) |> 
  unlist(recursive = FALSE) |> 
  sapply(\(x) rotate45(matrix(x))) |> 
  apply(1, identity, simplify = FALSE) |> 
  do.call(expand.grid, args = _) |> 
  apply(1, matrix, simplify = FALSE) |>
  lapply(rotate45, clockwise = FALSE) |> 
  Filter(x = _, \(x) all(abs(x - round(x)) < 0.01))

gap <- Filter(x = maybe_gaps, \(x) all(0 <= x & x <= 4000000) && !in_range(x))[[1]] 
format(gap[1] * 4000000 + gap[2], scientific = FALSE)
#>  [1] "11482462818989"
```

## day-14

``` r
input <- readLines("2022/day-14/input.txt") |> 
  strsplit(" -> |,") |> 
  lapply(\(x) lapply(2 * seq_len(length(x) / 2 - 1) - 1, \(i) {
    cbind(seq(x[i], x[i + 2]), seq(x[i + 1], x[i + 3]) + 1)
  })) |> 
  unlist(recursive = FALSE) |> 
  do.call(rbind, args = _) 

cave <- matrix(0L, nrow = max(input[,1]), ncol = max(input[,2]))
cave[input] <- 1L

solve <- function(cave, origin = cbind(500, 1)) {
  tries <- matrix(c(0, 1, -1, 1, 1, 1, 0, 0), ncol = 2, byrow = TRUE)
  pos <- pos2 <- origin
  repeat {
    for (i in 1:4) {
      pos2 <- pos + tries[i, , drop = FALSE]
      if (any(pos2 < c(1, 1) | dim(cave) < pos2)) return(cave)
      if (!cave[pos2]) pos <- pos2
      if (!cave[pos2]) break
    }
    if (i < 4) next 
    cave[pos] <- 2L
    if (cave[origin]) return(cave)
    pos <- pos2 <- origin
  }
}

# Part 1
sum(solve(cave) == 2)
#>  [1] 817

# Part 2
cave2 <- do.call(cbind, list(do.call(rbind, c(list(cave), rep(0, 200))), 0, 1))
sum(solve(cave2) == 2)
#>  [1] 23416
```

## day-13

``` r
input_raw <- readLines("2022/day-13/input.txt")

input <- input_raw |>
  gsub(x = _, "\\[", "list(") |>
  gsub(x = _, "\\]", ")") |>
  split(cumsum(input_raw == "")) |>
  lapply(\(x) lapply(setdiff(x, ""), \(y) eval(str2lang(y))))

is_in_order <- function(x, y) {
  for (i in seq_len(max(lengths(list(x, y))))) {
    if (i > length(x)) return(TRUE) else if (i > length(y)) return(FALSE)
    s <- if (is.list(c(x[[i]], y[[i]]))) is_in_order(x[[i]], y[[i]]) else sign(x[[i]] - y[[i]])
    if (is.logical(s)) return(s) else if (s < 0) return(TRUE) else if (s > 0) return(FALSE)
  }
  0
}

# Part 1
sum(which(sapply(input, \(x) is_in_order(x[[1]], x[[2]]))))
#>  [1] 5760

# Part 2
divs <- list(list(list(2)), list(list(6)))
elements <- c(unlist(input, FALSE), divs)
repeat {
  sorted <- Reduce(x = seq_len(length(elements) - 1), init = elements, \(x, i) {
    x[replace(seq_along(x), i + 0:1, i + if (is_in_order(x[[i]], x[[i + 1]])) 0:1 else 1:0)]
  }) 
  if (!identical(sorted, elements)) elements <- sorted else break
}

prod(which(sapply(sorted, \(x) any(sapply(divs, identical, x)))))
#>  [1] 26670
```

## day-12

``` r
input_raw    <- readLines("2022/day-12/input.txt")
width        <- nchar(input_raw[1])
input        <- unlist(strsplit(input_raw, ""))
start        <- match("S", input)
end          <- match("E", input)
input        <- match(input, letters)
input[start] <- 1
input[end]   <- 26

options <- lapply(seq_along(input), \(i) {
  opts <- c(
    if (i %% width != 1) i - 1,
    if (i %% width != 0) i + 1,
    if (i - width > 0) i - width,
    if (i + width < length(input)) i + width
  )
  opts[input[opts] <= input[i] + 1]
})

shortest_path <- function(from, to, steps) {
  r <- c(list(from), rep(list(NULL), length(steps) - 1))
  for (i in seq_along(steps)) {
    r[[i + 1]] <- x <- setdiff(unique(unlist(steps[r[[i]]])), unlist(r[seq_len(i)]))
    if (length(x) == 0) return(Inf)
    if (end %in% unlist(r)) break
  }
  length(Filter(\(x) !is.null(x), r[-1]))
}

# Part 1
shortest_path(from = start, to = end, steps = options)
#>  [1] 449

# Part 2
min(sapply(which(input == 1), shortest_path, end, options))
#>  [1] 443
```

## day-11

``` r
input_raw <- readLines("2022/day-11/input.txt")

input <- split(input_raw, cumsum(input_raw == "")) |> 
  lapply(setdiff, "") |> 
  lapply(\(x) list(
    items   = as.integer(strsplit(sub("\\D+", "", x[2]), ", ")[[1]]),
    divisor = as.integer(gsub("\\D", "", x[4])),
    op      = as.function(c(alist(old = ), str2lang(gsub("\\s*Operation: new = ", "", x[3])))),
    new     = as.function(c(alist(x = ), str2lang(do.call(sprintf, as.list(c(
      "ifelse(x %%%% %s == 0, %s, %s)", gsub("\\D", "", x[4:6])
    ))))))
  ))

big_divisor <- input |> sapply(\(x) x$divisor) |> prod()

solve <- function(input, times, divide = TRUE) {
  items <- numeric(length(input))
  Reduce(x = seq_len(times), init = input, function(x, y) {
    Reduce(x = seq_along(x), init = x, \(i, j) {
      cur <- i[[j]]
      items[j] <<- items[j] + length(cur$items)
      worry <- cur$op(cur$items)
      worry <- if (divide) floor(worry / 3) else worry %% big_divisor
      i[[j]]$items <- NULL
      for (w in worry) {
        new_monkey <- cur$new(w) + 1
        i[[new_monkey]]$items <- c(i[[new_monkey]]$items, w)
      }
      i
    }) 
  })
  prod(tail(sort(items), 2))
}

# Part 2
solve(input, 20, divide = TRUE)
#>  [1] 117640

# Part 2
solve(input, 10000, divide = FALSE)
#>  [1] 30616425600
```

## day-10

``` r
input <- unlist(strsplit(readLines("2022/day-10/input.txt"), " ")) |> 
  sapply(\(x) switch(x, noop = 0, addx = 0, as.numeric(x)), USE.NAMES = FALSE)

input <- head(cumsum(c(1, input)), -1)

# Part 1
indices <- 1:6 * 40 - 20
sum(input[indices] * indices)
#>  [1] 11220

# Part 2
matrix(input, ncol = 40, byrow = TRUE) |> 
  apply(1, \(x) paste(ifelse(x == -1:38 | x == 0:39 | x == 1:40, "#", " "), collapse = "")) |> 
  paste(collapse = "\n") |> 
  cat()
#>  ###  #### ###   ##    ## #### #    #  # 
#>  #  #    # #  # #  #    # #    #    # #  
#>  ###    #  #  # #  #    # ###  #    ##   
#>  #  #  #   ###  ####    # #    #    # #  
#>  #  # #    #    #  # #  # #    #    # #  
#>  ###  #### #    #  #  ##  #### #### #  #
```

## day-09

``` r
input <- inverse.rle(read.table("2022/day-09/input.txt", col.names = c("values", "lengths")))

stages <- Reduce(x = input, init = rep(list(c(0, 0)), 10), accumulate = TRUE, \(i, j) {
  i[[1]] <- i[[1]] + switch(j, R = c(1,  0), L = c(-1, 0), U = c(0, 1), D = c(0, -1))
  Reduce(\(h, t) if (all(abs(h - t) <= 1)) t else t + round((h - t) * 0.6), i, accumulate = TRUE)
})

# Part 1
length(unique(sapply(stages, \(x) paste(x[[2]], collapse = ","))))
#>  [1] 5735

# Part 2
length(unique(sapply(stages, \(x) paste(x[[10]], collapse = ","))))
#>  [1] 2478
```

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
