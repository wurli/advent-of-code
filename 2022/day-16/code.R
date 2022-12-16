input <- readLines("2022/day-16/test-input.txt") |> 
  strsplit("[^A-Z0-9]+") |> 
  lapply(\(x) list(name = x[2], rate = as.integer(x[3]), path = tail(x, -3)))

# 'Nodes' are just valves with flow > 0
names(input) <- sapply(input, `[[`, "name")
input <- lapply(input, `[`, -1)
nodes <- names(input[sapply(input, `[[`, "rate") > 0])

# Get all possible distances from one node to another
all_links <- function(node, len = 1, to_find = nodes, covered = NULL) {
  covered  <- union(covered, node) 
  to_find  <- setdiff(to_find, node) 
  links    <- input[[node]]$path
  found    <- intersect(to_find, links)
  to_find  <- setdiff(to_find, found)
  
  next_bit <- setdiff(links, covered)
  covered  <- union(covered, links)
  
  c(lapply(found, \(e) list(name = e, length = len)), 
    unlist(lapply(next_bit, all_links, len + 1, to_find, covered), recursive = FALSE))
}

# Get the shortest possible distances from one node to all the others
shortest_links <- function(node) {
  links <- all_links(node) 
  split(links, sapply(links, `[[`, "name")) |> 
    lapply(\(x) min(sapply(x, `[[`, "length"))) 
}

# Shortest possible distances from each node to the others, plus starting point
links <- sapply(c("AA", nodes), shortest_links, simplify = FALSE)

# Get all the paths between nodes which take <= `time` minutes
all_paths <- function(start = list("AA"), time = 30) {
  new <- lapply(start, \(p) {
    len <- length(p)
    if (len > 1) len <- len + sum(apply(embed(p, 2), 1, \(x) links[[rev(x)]]))
    opts <- links[[tail(p, 1)]]
    opts <- opts[!names(opts) %in% p]
    opts <- opts[vapply(opts, \(x) len + x <= time, logical(1))]
    names(opts)
  }) 
  
  done <- lengths(new) == 0
  if (all(done)) return(start)
  
  new <- which(!done) |> 
    lapply(\(i) lapply(new[[i]], \(p) c(start[[i]], p))) |> 
    unlist(recursive = FALSE) 
  
  c(start[done], all_paths(new, time = time))
}

# Get the total flow which occurs after `time` minutes for a given path
total_flow <- function(path, time = 30) {
  rates <- sapply(path, \(p) input[[p]]$rate)
  times <- time + 1 - cumsum(c(0, apply(embed(path, 2), 1, \(x) links[[rev(x)]])) + 1)
  sum(rates * times)
}

# Part 1
paths30 <- all_paths("AA", time = 30)
flows30 <- sapply(paths30, total_flow, time = 30)
max(flows30)

# Part 2
all_double_paths <- function(start1 = list("AA", "AA"), time = 30) {
  new <- lapply(start, \(p) {
    len <- length(p)
    if (len > 1) len <- len + sum(apply(embed(p, 2), 1, \(x) links[[rev(x)]]))
    opts <- links[[tail(p, 1)]]
    opts <- opts[!names(opts) %in% p]
    opts <- opts[vapply(opts, \(x) len + x <= time, logical(1))]
    names(opts)
  }) 
  
  done <- lengths(new) == 0
  if (all(done)) return(start)
  
  new <- which(!done) |> 
    lapply(\(i) lapply(new[[i]], \(p) c(start[[i]], p))) |> 
    unlist(recursive = FALSE) 
  
  c(start[done], all_paths(new, time = time))
}

names(paths26) <- unname(sapply(paths26, \(x) paste(x[-1], collapse = "_")))
flows26 <- lapply(paths26, total_flow, time = 26)

partition <- function(x, patterns, swap = FALSE) {
  if (length(patterns) == 0) return(list(x))
  test <- if (swap) !grepl(patterns[1], x) else grepl(patterns[1], x)
  c(partition(x[test],  tail(patterns, -1), swap = swap), 
    partition(x[!test], tail(patterns, -1), swap = !swap))
}

partition(names(paths26), nodes) |> 
  matrix(ncol = 2) |> 
  apply(1, identity) |> 
  Filter(x = _, \(x) all(lengths(x) > 0)) |>
  rapply(\(x) max(unlist(flows26[x])), how = "list") |> 
  sapply(\(x) sum(unlist(x))) |> 
  max()
# 2450 too low
