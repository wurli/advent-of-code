input <- readLines("2022/day-04/input.txt") |> 
  gsub("-", ",", x = _) |> 
  read.csv(text = _, col.names = c("x1", "x2", "y1", "y2"))

# Part 1
input |> 
  subset((x1 <= y1 & y2 <= x2) | (y1 <= x1 & x2 <= y2)) |> 
  nrow()

# Part 2
input |> 
  subset((x1 <= y1 & y1 <= x2) | (y1 <= x1 & x1 <= y2)) |> 
  nrow()
