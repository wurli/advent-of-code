input <- inverse.rle(read.table("2022/day-09/input.txt", col.names = c("values", "lengths")))

places_visited <- function(x, knots = 2) {
  stages <- Reduce(x = x, init = rep(list(c(0, 0)), knots), accumulate = TRUE, \(i, j) {
    i[[1]] <- i[[1]] + switch(j, R = c(1,  0), L = c(-1, 0), U = c(0, 1), D = c(0, -1))
    Reduce(\(h, t) if (all(abs(h - t) <= 1)) t else t + round((h - t) * 0.6), i, accumulate = TRUE)
  })
  length(unique(sapply(stages, \(x) paste(x[[knots]], collapse = ","))))
}

# Part 1
places_visited(input, knots = 2)

# Part 2
places_visited(input, knots = 10)
