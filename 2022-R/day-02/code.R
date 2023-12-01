library(tidyverse)

strategy <- as_tibble(read.table(
  "2022/day-02/input.txt",
  col.names = c("x", "y")
))

# Part 1
strategy |> 
  mutate(
    across(
      c(x, y), 
      ~ case_when(
        . %in% c("A", "X") ~ 1,
        . %in% c("B", "Y") ~ 2,
        . %in% c("C", "Z") ~ 3
      )
    ),
    result = (y - x) %% 3,
    result = case_when(
      result == 1 ~ 6,
      result == 2 ~ 0,
      result == 0 ~ 3
    ),
    score = y + result
  ) |> 
  summarise(
    score = sum(score)
  )

# Part 2
strategy |> 
  mutate(
    x = case_when(
      x %in% c("A", "X") ~ 1,
      x %in% c("B", "Y") ~ 2,
      x %in% c("C", "Z") ~ 3
    ),
    result = case_when(
      y == "X" ~ 2, # loss
      y == "Y" ~ 0, # draw
      y == "Z" ~ 1  # win
    ),
    y = (x + result - 1) %% 3 + 1,
    result = case_when(
      result == 1 ~ 6,
      result == 2 ~ 0,
      result == 0 ~ 3
    ),
    score = y + result
  ) |> 
  summarise(
    score = sum(score)
  )
