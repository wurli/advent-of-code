library(tidyverse)

nums <- readr::read_lines("rust_2021/inputs/day01.txt") |> 
  as.integer()

n_increases <- function(nums, window = 1) {
  
  window <- window - 1
  
  vals <- seq_len(length(nums) - window) |> 
    map_int(\(i) sum(nums[i + seq(0, window)]))
  
  map2_int(head(vals, -1), tail(vals, -1), ~ .x < .y) |> 
    sum()
  
}

n_increases(nums, window = 1)
n_increases(nums, window = 3)
