sym <- function (x) {
  if (is.symbol(x)) {
    return(x)
  }
  if (identical(x, "")) {
    return(quote(expr = ))
  }
  if (!is_string(x)) {
    stop("Cannot coerce input to a symbol")
  }
  as.symbol(x)
}

as_syms <- function(x) {
  lapply(x, sym)
}

as_syms2 <- function(...) {
  lapply(list(...), sym)
}
