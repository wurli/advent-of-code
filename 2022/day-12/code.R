input_raw <- readLines("2022/day-12/input.txt")
width <- nchar(input_raw[1])

input <- unlist(strsplit(input_raw, ""))

start <- match("S", input)
end   <- match("E", input)

input <- match(input, letters)
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
    r[[i + 1]] <- x <- setdiff(unique(unlist(options[r[[i]]])), unlist(r[seq_len(i)]))
    if (length(x) == 0) return(Inf)
    if (end %in% unlist(r)) break
  }
  length(Filter(\(x) !is.null(x), r[-1]))
}

# Part 1
shortest_path(from = start, to = end, steps = options)

# Part 2
min(sapply(which(input == 1), shortest_path, end, options))
