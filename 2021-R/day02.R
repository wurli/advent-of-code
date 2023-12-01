library(tidyverse)

input <- read_lines("rust_2021/inputs/day02.txt") |> 
  strsplit(" ")

# P1
pos <- c(0, 0)

input |> 
  walk(\(x) {
    type   <- x[1]
    amount <- x[2] |> as.integer() 
    pos <<- pos + switch(x[1],
      up       = c(0, -amount),
      down     = c(0, amount),
      forward  = c(amount, 0)
    )
  })

prod(pos)

# P2
pos <- c(0, 0, 0)

input |> 
  walk(\(x) {
    type   <- x[1]
    amount <- x[2] |> as.integer() 
    pos <<- pos + switch(x[1],
      up       = c(0, 0, -amount),
      down     = c(0, 0, amount),
      forward  = c(amount, amount * pos[3], 0)
    )
  })

prod(pos[1:2])
