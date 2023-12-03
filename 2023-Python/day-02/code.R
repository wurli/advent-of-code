input <- readLines("2023-Python/day-02/input.txt") 

str_extract <- function(x, pattern, default = "0") {
  regmatches(x, regexec(pattern, x, perl = TRUE)) |> 
    lapply(\(x) if (length(x) == 0L) default else x) |> 
    unlist()
}

# -- Parsing -------------------------------------------------------------------
input_parsed <- input |> 
  lapply(function(line) {
    parts <- strsplit(line, ": ")[[1]]
    sets  <- strsplit(parts[2], "; ")[[1]]
    data.frame(
      game_id = as.integer(str_extract(parts[1], "(?<=Game )\\d+")),
      n_red   = as.integer(str_extract(sets, "\\d+(?= red)")), 
      n_green = as.integer(str_extract(sets, "\\d+(?= green)")),
      n_blue  = as.integer(str_extract(sets, "\\d+(?= blue)"))
    )
  }) |> 
  do.call(args = _, rbind)

max_cubes <- input_parsed |> 
  aggregate(by = list(input_parsed$game_id), max) 

# -- Part 1 --------------------------------------------------------------------
with(max_cubes, sum(unique(game_id[n_red < 12 & n_green < 13 & n_blue < 14])))

# -- Part 2 --------------------------------------------------------------------
with(max_cubes, sum(n_red * n_green * n_blue))
