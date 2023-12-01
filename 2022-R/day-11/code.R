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

# Part 2
solve(input, 10000, divide = FALSE)