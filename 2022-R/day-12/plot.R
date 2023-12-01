input_raw    <- readLines("2022/day-12/input.txt")
width        <- nchar(input_raw[1])
input        <- unlist(strsplit(input_raw, ""))
start        <- match("S", input)
end          <- match("E", input)
input        <- match(input, letters)
input[start] <- 1
input[end]   <- 26

options <- lapply(seq_along(input), \(i) {
  opts <- c(
    if (i %% width != 1) i - 1,
    if (i %% width != 0) i + 1,
    if (i - width > 0) i - width,
    if (i + width < length(input)) i + width
  )
  opts[input[opts] <= input[i] + 1]
})

shortest_path <- function(from, to, steps) {
  r <- c(list(from), rep(list(NULL), length(steps) - 1))
  for (i in seq_along(steps)) {
    r[[i + 1]] <- x <- setdiff(unique(unlist(steps[r[[i]]])), unlist(r[seq_len(i)]))
    if (length(x) == 0) return(Inf)
    if (end %in% unlist(r)) break
  }
  r[!sapply(r, is.null)]
}

# Part 1
fill <- shortest_path(from = start, to = end, steps = options)

library(tidyverse)
library(gganimate)

fill_data <- fill |> 
  map(~ tibble(row = .)) |> 
  bind_rows(.id = "stage") |> 
  mutate(
    x = (row - 1) %% width + 1,
    y = (row - 1) %/% width,
    stage = as.integer(stage)
  )

anim <- tibble(height = input) |> 
  mutate(
    x = (row_number() - 1) %% width + 1,
    y = (row_number() - 1) %/% width
  ) |> 
  ggplot(aes(x, y, fill = height)) +
  geom_tile(show.legend = FALSE) + 
  geom_tile(
    fill = "white",
    # colour = "grey90",
    alpha = 0.4,
    size = 0.1,
    data = fill_data
  ) +
  theme_void() +
  transition_states(stage, transition_length = 1) +
  shadow_mark(exclude_layer = 1) +
  annotate(
    "tile",
    fill = "red",
    colour = "yellow",
    x = (start - 1) %% width + 1,
    y = (start - 1) %/% width
  ) +
  annotate(
    "tile",
    fill = "red",
    colour = "yellow",
    x = (end - 1) %% width + 1,
    y = (end - 1) %/% width
  ) +
  coord_fixed()

anim_save("2022/day-12/animation.gif", anim)
