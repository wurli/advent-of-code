input <- readLines("2022/day-15/input.txt") |> 
  sapply(\(x) as.integer(strsplit(x, "[^0-9-]+")[[1]][-1]), USE.NAMES = FALSE) |> 
  t()

taxi_dist <- function(x1, y1, x2, y2) abs(x1 - x2) + abs(y1 - y2) 
distances <- taxi_dist(input[,1], input[,2], input[,3], input[,4])

x <- seq_len(nrow(input)) |> 
  lapply(\(i) {
    
    row <- 2000000
    sensor_row <- input[,2][i]
    sensor_col <- input[,1][i]
    len <- distances[i]
    
    h_len <- len - abs(sensor_row - row)
    # sensor_col + h_len * c(-1, 1)
    seq(sensor_col - h_len, sensor_col + h_len)
    
  }) |> 
  Reduce(x = _, union) 



sum(min(input[,c(1, 3)]) <= x & x <= max(input[,c(1, 3)])) - nrow(unique(input[,3:4][input[,4] == 2000000, , drop = FALSE]))

# 4390364 Too low
# 4390363
# 4798789 Too high
# 6441790 Too high