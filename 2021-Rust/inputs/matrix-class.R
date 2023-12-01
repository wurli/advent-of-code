library(S7)

mat <- new_class("mat",
  properties = list(
    content = class_vector,
    nrow = class_integer,
    ncol = new_property(
      getter = function(self) length(self@content) %/% self@nrow 
    )
  ),
  validator = function(self) {
    stopifnot(
      0 < self@nrow, self@nrow <= length(self@content),
      length(self@content) %% self@nrow == 0L
    )
  }
)

get_row <- new_generic("get_row", "x")

method(get_row, mat) <- function(x, n) {
  stopifnot(0 < n, n <= x@nrow)
  x@content[seq(x@ncol * (n - 1) + 1, x@ncol * n)]
}

get_col <- new_generic("get_col", "x")

method(get_col, mat) <- function(x, n) {
  i <- seq_along(x@content)
  stopifnot(0 < n, n <= x@ncol)
  x@content[seq(n, (x@nrow - 1) * x@ncol + n, by = x@ncol)]
}

transpose <- new_generic("transpose", "x")

method(transpose, mat) <- function(x) {
  new_content <- seq_len(x@ncol) |>
    lapply(\(r) get_col(x, r)) |> 
    unlist(use.names = FALSE)
  
  x |> 
    set_props(
      content = new_content,
      nrow = x@ncol
    )
}

method(print, mat) <- function(x, ...) {
  elements <- format(x@content)
  lens <- nchar(elements)
  elements <- paste0(strrep(" ", lens - max(lens)), elements)
  
  elements <- paste0(ifelse(seq_along(elements) %% x@ncol == 1L, "\n", "  "), elements)
  
  cat("A matrix ", x@nrow, " x ", x@ncol, "\n", sep = "")
  cat(elements, sep = "")
}

m <- mat(1:15, 5L)
m
m |> get_row(1)

m |> get_col(4)
m@ncol
m |> transpose()
