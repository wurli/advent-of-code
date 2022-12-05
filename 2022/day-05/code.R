crates_raw       <- readLines("2022/day-05/crates.txt")
instructions_raw <- readLines("2022/day-05/instructions.txt")

crates <- crates_raw |> 
  strsplit("") |> 
  vapply(\(x) x[((0:8) * 4) + 2], character(9)) |> 
  lapply(y = _, X = 1:9, FUN = \(x, y) y[x,][y[x,] != " "])

instructions <- instructions_raw |> 
  regmatches(gregexpr("\\d+", instructions_raw)) |> 
  lapply(as.numeric) 

rearrange <- function(crates, instructions, reverse = TRUE) {
  reverse <- if (reverse) rev else identity
  
  for (ins in instructions) {
    crates[[ins[3]]] <- c(
      reverse(head(crates[[ins[2]]], ins[1])),
      crates[[ins[3]]]
    )
    crates[[ins[2]]] <- tail(crates[[ins[2]]], -ins[1])
  }
  
  crates 
}

# Part 1
crates |> 
  rearrange(instructions, reverse = TRUE) |> 
  vapply(head, character(1), 1) |> 
  paste(collapse = "")

# Part 2
crates |> 
  rearrange(instructions, reverse = FALSE) |> 
  vapply(head, character(1), 1) |> 
  paste(collapse = "")
