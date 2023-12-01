input <- unlist(strsplit(readLines("2022/day-10/input.txt"), " ")) |> 
  sapply(\(x) switch(x, noop = 0, addx = 0, as.numeric(x)), USE.NAMES = FALSE)

input <- head(cumsum(c(1, input)), -1)

# Part 1
indices <- 1:6 * 40 - 20
sum(input[indices] * indices)

# Part 2
matrix(input, ncol = 40, byrow = TRUE) |> 
  apply(1, \(x) paste(ifelse(x == -1:38 | x == 0:39 | x == 1:40, "#", " "), collapse = "")) |> 
  paste(collapse = "\n") |> 
  cat()
