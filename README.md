Solutions to Advent of Code 2022
================
Jacob Scott

These are my solutions to [Advent of Code](https://adventofcode.com/)
2022.

## day-01

``` r
library(tidyverse)

input <- readr::read_file("2022/day-01/input.txt") 

total_calories <- input |> 
    strsplit("\n\n") |> 
    pluck(1) |> 
    map_dbl(\(x) {
        
        x <- strsplit(x, "\n")[[1]]
        
        x |> 
            as.numeric() |> 
            sum()
        
    }) |> 
    sort(decreasing = TRUE) 

# Part 1
total_calories[1]
#>  [1] 71023

# Part 2
sum(total_calories[1:3])
#>  [1] 206289
```
