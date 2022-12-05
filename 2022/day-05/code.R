crates_raw       <- read.fwf("2022/day-05/crates.txt", rep(1, 9 * 4 - 1))
instructions_raw <- readLines("2022/day-05/instructions.txt")

crates <- crates_raw[((0:8) * 4) + 2] |> 
  lapply(\(x) x[x != " "])

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
