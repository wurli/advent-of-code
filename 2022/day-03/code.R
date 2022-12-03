library(tidyverse)

input <- read_lines("2022/day-03/input.txt")

priorities <- input |> 
  strsplit("") |> 
  map(
    ~ .x |> 
      str_replace_all(
        1:52 |> 
          as.character() |> 
          set_names(c(letters, LETTERS)) 
      ) |> 
      as.numeric()
  )

# Part 1
priorities |> 
  map_dbl(\(items) {
    
    intersect(
      items[seq_along(items) <= length(items) / 2],
      items[seq_along(items) >  length(items) / 2]
    )
    
  }) |> 
  sum()

# Part 2
priorities |> 
  tapply(
    (seq_along(input) - 1) %/% 3,
    function(x) reduce(x, intersect)
  ) |> 
  sum()
