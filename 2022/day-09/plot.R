library(tidyverse)
library(gganimate)

input <- inverse.rle(read.table("2022/day-09/input.txt", col.names = c("values", "lengths")))

stages <- Reduce(x = input, init = rep(list(c(0, 0)), 10), accumulate = TRUE, \(i, j) {
  i[[1]] <- i[[1]] + switch(j, R = c(1,  0), L = c(-1, 0), U = c(0, 1), D = c(0, -1))
  Reduce(\(h, t) if (all(abs(h - t) <= 1)) t else t + round((h - t) * 0.6), i, accumulate = TRUE)
})

plot_data <- stages |> 
  map_dfr(as_tibble, .name_repair = ~ paste0("knot_", seq_along(.))) |> 
  mutate(
    coord = rep(c("x", "y"), n() / 2),
    stage = (row_number() + 1) %/% 2
  ) |> 
  pivot_longer(
    starts_with("knot"), 
    names_prefix = "knot_", 
    names_to = "knot", 
    values_to = "pos"
  ) |> 
  pivot_wider(
    names_from = coord, 
    values_from = pos
  )

anim <- plot_data |> 
  mutate(fill = ifelse(knot == "10", "grey10", "grey40")) |> 
  ggplot(aes(x, y, fill = fill, group = stage)) + 
  geom_tile(data = ~ .x |> filter(knot != "10")) +
  geom_tile(data = ~ .x |> filter(knot == "10")) +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect("lightblue", "transparent")) +
  transition_states(stage) +
  shadow_mark(exclude_layer = 1, fill = "grey90")

anim_save("2022/day-09/animation.gif", animate(anim, fps = 100))
