library(tidyverse)

input <- readr::read_file("2022-01/2022-01-input.txt") 

total_calories <- input |> 
	strsplit("\n\n") |> 
	pluck(1) |> 
	map_dbl(\(x) {
		
		x <- strsplit(x, "\n")[[1]]
		
		x |> 
			as.numeric() |> 
			sum()
		
	}) |> 
	sort(decreasing = TRUE) 

# Part 1
total_calories[1]

# Part 2
sum(total_calories[1:3])
