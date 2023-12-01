input <- strsplit(readLines("2022/day-06/input.txt"), "")[[1]]

first_marker <- function(x, len = 4) {
  len - 1 + match(TRUE, sapply(
    seq_len(length(x) - len),
    \(i) !any(duplicated(x[seq_len(len) + i - 1]))
  ))
}

# Part 1
first_marker(input, len = 4)

# Part 2
first_marker(input, len = 14)