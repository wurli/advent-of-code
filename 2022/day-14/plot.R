show_grid <- function(grid) {
  require(tidyverse)
  
  grid |> 
    as_tibble(.name_repair = ~ paste0("x", rev(seq_along(.x)))) |> 
    mutate(row = row_number()) |> 
    pivot_longer(starts_with("x"), names_prefix = "x", names_transform = as.integer) |> 
    ggplot(aes(row, name, fill = value, colour = value)) +
    geom_tile(show.legend = FALSE) +
    coord_fixed() +
    theme_void() +
    theme(panel.background = element_rect("black", NA))
  
}
