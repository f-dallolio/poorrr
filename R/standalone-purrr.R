
map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

map_lgl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  .rlang_purrr_map_mold(.x, .f, character(1), ...)
}
.rlang_purrr_map_mold <- function(.x, .f, .mold, ...) {
  .f <- as_function(.f, env = global_env())
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}

map2 <- function(.x, .y, .f, ...) {
  .f <- as_function(.f, env = global_env())
  out <- mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
  if (length(out) == length(.x)) {
    set_names(out, names(.x))
  } else {
    set_names(out, NULL)
  }
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}

walk2 <- function(.x, .y, .f, ...) {
  map2(.x, .y, .f, ...)
  invisible(.x)
}

imap <- function(.x, .f, ...) {
  map2(.x, names(.x) %||% seq_along(.x), .f, ...)
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

pmap <- function(.l, .f, ...) {
  .f <- as.function(.f)
  args <- .rlang_purrr_args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
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

.rlang_purrr_args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}

keep <- function(.x, .f, ...) {
  .x[.rlang_purrr_probe(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- .rlang_purrr_probe(.x, .p, ...)
  .x[is.na(sel) | !sel]
}
map_if <- function(.x, .p, .f, ..., .else = NULL) {
  matches <- .rlang_purrr_probe(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  if (is.null(.else)) {
    return(.x)
  }
  .x[!matches] <- map(.x[!matches], .else, ...)
  .x
}
.rlang_purrr_probe <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = global_env())
    map_lgl(.x, .p, ...)
  }
}

compact <- function(.x) {
  .x[as.logical(lengths(.x))]
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

every <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (!rlang::is_true(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (rlang::is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
negate <- function(.p) {
  .p <- as_function(.p, env = global_env())
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) .f(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .rlang_purrr_index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
.rlang_purrr_index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

list_c <- function(x) {
  do.call("c", x)
}






