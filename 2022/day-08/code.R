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