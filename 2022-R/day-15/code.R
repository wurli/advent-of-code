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