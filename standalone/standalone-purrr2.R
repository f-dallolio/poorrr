iwalk <- function(.x, .f, ...) {
  imap(.x, .f, ...)
  invisible(.x)
}
imap_lgl <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "logical")
}
imap_int <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "integer")
}
imap_dbl <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "double")
}
imap_chr <- function(.x, .f, ...) {
  as.vector(imap(.x, .f, ...), "character")
}
imap_vec <- function(.x, .f, ..., .ptype = NULL) {
  out <- imap(.x, .f, ...)
  list_simplify(out, ptype = .ptype)
}

list_transpose <- function(.l) {
  if (!length(.l)) {
    return(.l)
  }

  inner_names <- names(.l[[1]])

  if (is.null(inner_names)) {
    fields <- seq_along(.l[[1]])
  } else {
    fields <- set_names(inner_names)
    .l <- map(.l, function(x) {
      if (is.null(names(x))) {
        set_names(x, inner_names)
      } else {
        x
      }
    })
  }

  # This way missing fields are subsetted as `NULL` instead of causing
  # an error
  .l <- map(.l, as.list)

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

map_at <- function(.x, .at, .f, ...) {
  where <- where_at(.x, .at)
  out <- vector("list", length(.x))
  out[where] <- map(.x[where], .f, ...)
  out[!where] <- .x[!where]
  setNames(out, names(.x))
}
map_vec <- function(.x, .f, ...) { # .ptype = NULL) {
  out <- map(.x, .f, ...)
  list_simplify0(out) # , ptype = .ptype)
}

walk2 <- function(.x, .y, .f, ...) {
  map2(.x, .y, .f, ...)
  invisible(.x)
}
map2_vec <- function(.x, .y, .f, ..., .ptype = NULL) {
  out <- map2(.x, .y, .f, ...)
  list_simplify(out, ptype = .ptype)
}

none <- function(.x, .p, ...) {
  !some(.x, .p, ...)
}

pwalk <- function(.l, .f, ...) {
  pmap(.l, .f, ...)
  invisible(.l)
}
pmap_lgl <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "logical")
}
pmap_int <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "integer")
}
pmap_dbl <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "double")
}
pmap_chr <- function(.l, .f, ...) {
  as.vector(pmap(.l, .f, ...), "character")
}
pmap_vec <- function(.l, .f, ...) {
  out <- pmap(.l, .f, ...)
  list_simplify(out, ptype = .ptype)
}
