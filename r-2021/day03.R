library(tidyverse)

input <- read_lines("rust_2021/inputs/day03.txt") |> 
  strsplit("") |> 
  map(as.integer) |> 
  do.call(args = _, rbind)

most_common_bit  <- function(x) as.integer(sum(x == 1) >= sum(x == 0))
least_common_bit <- function(x) invert(most_common_bit(x))
invert           <- function(x) as.integer(!as.logical(x))
bin2dec          <- function(x) sum(imap_int(rev(x), ~ .x * 2L^(.y - 1L)))

most_common_bits <- apply(input, 2, most_common_bit)

# Part 1
bin2dec(most_common_bits) * bin2dec(invert(most_common_bits))

# P2 ------------

solve_p2 <- function(x, test) {
  bin2dec(as.integer(reduce(seq_len(ncol(x)), .init = x, \(x, i) {
    if (nrow(x) == 1) return(x)
    x[x[, i] == test(x[, i]), , drop = FALSE]
  })))
}

solve_p2(input, most_common_bit) * solve_p2(input, least_common_bit) 
