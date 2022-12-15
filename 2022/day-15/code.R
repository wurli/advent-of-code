input <- readLines("2022/day-15/test-input.txt") |> 
  sapply(\(x) as.integer(strsplit(x, "[^0-9-]+")[[1]][-1]), USE.NAMES = FALSE) |> 
  t()

taxi_dist <- function(x1, y1, x2, y2) abs(x1 - x2) + abs(y1 - y2) 
distances <- taxi_dist(input[,1], input[,2], input[,3], input[,4])

x <- sapply(
  seq(min(input[, c(1, 3)]), max(input[, c(1, 3)])), 
  \(i) any(taxi_dist(input[,1], input[,2], i, 10) <= distances)
)

sum(x) - nrow(unique(input[,3:4][input[,4] == 10, , drop = FALSE]))

library(tidyverse)

tibble(row = seq(min(input[, c(1, 3)]), max(input[, c(1, 3)])), fill = x) |> 
  ggplot(aes(row, 1, fill = fill)) +
  geom_tile(colour = "black") +
  coord_fixed()

# 4390364 WRONG
# 4390363 WRONG
# 4036528 WRONG
# 4798789 WRONG - too high