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
