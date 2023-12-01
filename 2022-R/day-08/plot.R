library(tidyverse)

input <- readLines("2022/day-08/input.txt") |> 
  strsplit("") |>
  sapply(as.numeric, USE.NAMES = FALSE) 

plot_trees <- function(x, scale = 1) {
  x |> 
    as_tibble(.name_repair = NULL) |> 
    mutate(row = row_number()) |> 
    pivot_longer(-row) |> 
    ggplot(aes(row, name, fill = value ^ scale)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_gradient(low = "black", high = "yellow") +
    theme_void() +
    theme(panel.background = element_rect("black", colour = NULL)) +
    coord_fixed()
}

input |> 
  plot_trees(2)

rep(list(input), 4) |> 
  rotate_each(clockwise = TRUE) |> 
  lapply(apply, 2, \(x) sapply(seq_along(x), \(i) {
    unblocked <- x[rev(seq_len(i - 1))] < x[i]
    sum(cumprod(unblocked)) + !all(unblocked)
  })) |> 
  rotate_each(clockwise = FALSE) |> 
  Reduce(x = _, f = `*`, init = 1) |> 
  plot_trees(scale = 1 / 4)
