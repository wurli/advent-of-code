input <- inverse.rle(read.table("2022/day-09/input.txt", col.names = c("values", "lengths")))

stages <- Reduce(x = input, init = rep(list(c(0, 0)), 10), accumulate = TRUE, \(i, j) {
  i[[1]] <- i[[1]] + switch(j, R = c(1,  0), L = c(-1, 0), U = c(0, 1), D = c(0, -1))
  Reduce(\(h, t) if (all(abs(h - t) <= 1)) t else t + round((h - t) * 0.6), i, accumulate = TRUE)
})

# Part 1
length(unique(sapply(stages, \(x) paste(x[[2]], collapse = ","))))

# Part 2
length(unique(sapply(stages, \(x) paste(x[[10]], collapse = ","))))