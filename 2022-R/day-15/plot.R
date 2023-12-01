library(tidyverse)

intersections <- seq_len(ncol(input)) |> 
  lapply(\(i) lapply(list(c(1, 0), c(-1, 0)), \(x) input[,i][1:2] + x * (distances[i] + 1))) |> 
  unlist(recursive = FALSE) |> 
  sapply(\(x) rotate45(matrix(x))) |> 
  # cbind(matrix(c(1, 1, 19, 19), nrow = 2)) |>
  apply(1, identity, simplify = FALSE) |> 
  do.call(expand.grid, args = _) |> 
  apply(1, matrix, simplify = FALSE) |>
  lapply(rotate45, clockwise = FALSE) |> 
  Filter(x = _, \(x) all(abs(x - round(x)) < 0.01))

lapply(seq_len(ncol(input)), \(i) {
  lapply(list(c(0, 1), c(1, 0), c(0, -1), c(-1, 0)), \(x) input[,i][1:2] + x * distances[i])
}) |> 
  map(as_tibble, .name_repair = ~ paste0("x", seq_along(.))) |> 
  map(mutate, row = row_number()) |> 
  map(pivot_longer, -row) |> 
  map(pivot_wider, names_from = row, names_prefix = "x") |> 
  bind_rows(.id = "shape") |> 
  ggplot(aes(x = x1, y = x2, group = shape)) +
  geom_polygon(
    fill = "black",
    data = tibble(
      x1 = c(0, 20, 20, 0),
      x2 = c(20, 20, 0, 0),
      shape = -1
    ),
    alpha = 0.9
  ) +
  geom_polygon(fill = "lightblue", alpha = 0.8) +
  geom_point(
    colour = "red",
    data = t(do.call(cbind, intersections)) |> 
      as_tibble(.name_repair = ~ c("x1", "x2")) |> 
      mutate(shape = -2)
  ) + 
  scale_x_continuous(
    breaks = ~ floor(seq(.[1], .[2])),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    breaks = ~ floor(seq(.[1], .[2])),
    minor_breaks = NULL
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    panel.ontop = TRUE
  )
