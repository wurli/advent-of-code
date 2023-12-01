library(tidyverse)
input <- read_lines("rust_2021/inputs/day04.txt")
draws <- input[1] |> str_split_1(",") |> as.integer()

# boards <- input[-(1:2)] |> 
#   str_squish() |> 
#   paste(collapse = "\n") |> 
#   str_split_1("\n\n") |> 
#   str_split("\n") |> 
#   map(str_split, "\\s+") |> 
#   map(map, as.integer) |>
#   map(~ do.call(rbind, args = .))

boards <- input[-(1:2)] |>
  str_squish() |> 
  paste(collapse = "\n") |>
  str_replace_all(" +", ",") |> 
  str_split_1("\n\n") |> 
  map(\(x) read.csv(text = x, header = FALSE) |> as.matrix() |> unname()) 

for (di in seq_along(draws)) {
  d            <- draws[di]
  should_check <- any(di >= dim(boards[[1]]))
  
  boards <- boards |>
    map(\(board) {
      board[board == d] <- NA
      board
    })
  
  if (!should_check) next
  
  check <- boards |> 
    map_lgl(\(b) {
      any(
        b |> apply(1, \(row) all(is.na(row))),
        b |> apply(2, \(col) all(is.na(col)))
      )
    })
  
  if (!any(check)) next
  
  winning_board <- boards[[which(check)]]
  
  break
}

sum(winning_board, na.rm = TRUE) * d
