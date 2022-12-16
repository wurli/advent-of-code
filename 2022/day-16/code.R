input <- readLines("2022/day-16/input.txt") |> 
  strsplit("[^A-Z0-9]+") |> 
  lapply(\(x) list(name = x[2], rate = as.integer(x[3]), path = tail(x, -3)))

names(input) <- sapply(input, `[[`, "name")
input <- lapply(input, `[`, -1)
nodes <- names(input[sapply(input, `[[`, "rate") > 0])

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

shortest_links <- function(node) {
  links <- find_all_links(node) 
  split(links, sapply(links, `[[`, "name")) |> 
    lapply(\(x) min(sapply(x, `[[`, "length"))) 
}

links <- sapply(c("AA", nodes), shortest_links, simplify = FALSE)

all_paths <- function(start = list("AA"), max_len = 30) {
  
  pts <- lapply(start, \(p) {
    len <- length(p)
    if (len > 1) len <- len + sum(apply(embed(p, 2), 1, \(x) links[[rev(x)]]))
    
    opts <- links[[tail(p, 1)]]
    opts <- opts[!names(opts) %in% p]
    opts <- opts[vapply(opts, \(x) len + x <= max_len, logical(1))]
    names(opts)
  }) 
  
  done <- lengths(pts) == 0
  if (all(done)) return(start)
  
  new <- which(!done) |> 
    lapply(\(i) lapply(pts[[i]], \(p) c(start[[i]], p))) |> 
    unlist(recursive = FALSE) 
  
  c(start[done], all_paths(new))
  
}

paths <- all_paths("AA")

# # Best path
# path <- c("AA", "DD", "BB", "JJ", "HH", "EE", "CC")

max(sapply(paths, \(path) {
  
  rates <- sapply(path, \(p) input[[p]]$rate)
  times <- 31 - cumsum(c(0, apply(embed(path, 2), 1, \(x) links[[rev(x)]])) + 1)
  sum(rates * times)
  
}))

