# Create an actual shell script using the input
readLines("2022/day-07/input.txt") |> 
  gsub(x = _, "/", "fs") |> 
  gsub(x = _, "\\$ cd \\.\\.", "cd ..") |> 
  gsub(x = _, "\\$ cd ([^.]+)", "mkdir \\1 && cd \\1") |> 
  gsub(x = _, "^(\\d+).+", 'echo "\\1" >> sizes.txt') |> 
  gsub(x = _, "^(\\$|dir).+", "") |> 
  writeLines("instructions.sh")

# Execute the shell script to create a file structure
system2("bash", "instructions.sh")

# Get the total 'sizes' of each directory
sizes <- list.dirs("fs", recursive = TRUE) |> 
  sapply(\(dir) dir |> 
    list.files(full.names = TRUE, recursive = TRUE) |> 
    sapply(\(x) sum(scan(x, quiet = TRUE))) |> 
    sum()
  )

# Remove the shell script and file structure
unlink(c("fs", "instructions.sh"), recursive = TRUE)

# Part 1
sum(sizes[sizes <= 100000])

# Part 2
min(sizes[sizes >= 30000000 - (70000000 - max(sizes))])