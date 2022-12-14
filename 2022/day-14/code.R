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

# Part 2
cave2 <- do.call(cbind, list(do.call(rbind, c(list(cave), rep(0, 200))), 0, 1))
sum(solve(cave2) == 2)
