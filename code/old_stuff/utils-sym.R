sym <- function(x) {
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


(function(...) {
  x <- lapply(paste0("..", seq_len(...length())), as.symbol)
  for (i in seq_along(x)) {
    x[[i]] <- eval(substitute(x[[i]]), parent.frame())
  }
  x
})(mean, var)


all_values <- function(.values, ...) {
  if (missing(.values)) {
    values <- list(...)
  } else if (identical(.values, globalenv())) {
    # substitute doesn't want to replace in globalenv
    values <- as.list(globalenv())
  } else {
    values <- .values
  }

  if (is.list(values)) {
    # Replace lazy objects with their expressions
    is_lazy <- vapply(values, is.lazy, logical(1))
    values[is_lazy] <- lapply(values[is_lazy], `[[`, "expr")
  }

  values
}
