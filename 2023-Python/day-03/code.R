library(purrr)
library(stringr)

input <- readLines("2023-Python/day-03/input.txt")
input_parsed <- do.call(rbind, strsplit(input, ""))

get_coords <- function(x, pattern) {
  coords <- x |> 
    str_locate_all(pattern) |> 
    imap(function(x, i) {
      apply(x, 1, function(x) unname(cbind(i, seq(x[1], x[2]))), simplify = FALSE)
    }) |> 
    list_flatten() |> 
    compact()
  
  names(coords) <- map_chr(coords, function(x) paste(input_parsed[x], collapse = ""))
  
  coords
}

expand_coords <- function(coords, dim) {
  first <- head(coords, 1)
  last  <- tail(coords, 1)
  out <- rbind(
    cbind(coords[, 1] - 1, coords[, 2]),
    cbind(coords[, 1] + 1, coords[, 2]),
    cbind(first[, 1] + -1:1, first[, 2] - 1),
    cbind(last[, 1] + -1:1, last[, 2] + 1)
  )
  rows <- out[, 1]
  cols <- out[, 2]
  out[0 < rows & rows <= dim[1] & 0 < cols & cols <= dim[2], , drop = FALSE]
}

num_coords  <- input |> get_coords("\\d+")
part_coords <- input |> get_coords("[^.0-9]")

# -- Part 1 --------------------------------------------------------------------
input |> 
  get_coords("\\d+") |> 
  keep(function(coords) {
    surroundings <- input_parsed[expand_coords(coords, dim(input_parsed))]
    any(!surroundings %in% c(".", 0:9))
  }) |> 
  names() |> 
  as.integer() |> 
  sum()

# -- Part 2 --------------------------------------------------------------------
input |> 
  get_coords("[*]") |> 
  map_int(function(coords) {
    expanded <- expand_coords(coords, dim(input_parsed))
    adjacent_nums <- num_coords |> keep(function(num) nrow(vctrs::vec_set_intersect(num, expanded)) > 0)
    if (length(adjacent_nums) == 2L) prod(as.integer(names(adjacent_nums))) else NA_integer_
  }) |> 
  sum(na.rm = TRUE)
