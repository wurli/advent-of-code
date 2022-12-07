input <- readLines("2022/day-07/input.txt")

# 1. Parse console log to get a representation of the filesystem
fs <- Reduce(x = input, init = list(fs = list(), pos = NULL), \(x, y) {
  
  if (grepl("^\\$ cd [^.]"  , y)) x$pos <- c(x$pos, sub("^\\$ cd ", "", y))
  if (grepl("^\\$ cd \\.\\.", y)) x$pos <- head(x$pos, -1)
  
  x$fs[[x$pos]] <- c(x$fs[[x$pos]], if (!grepl("^\\$", y)) setNames(
    list(if (!grepl("^dir", y)) as.integer(gsub("\\D", "", y))), 
    sub("^.+ ", "", y))
  )
  
  x
  
})$fs

# 2. Get size of each directory
get_sizes <- function(x) Reduce(
  \(i, j) c(i, setNames(sum(unlist(x[[j]])), j), get_sizes(x[[j]])), 
  names(Filter(is.list, x)), NULL
)

sizes <- get_sizes(fs)

# Part 1
sum(sizes[sizes <= 100000])

# Part 2
min(sizes[sizes >= 30000000 - (70000000 - max(sizes))])
