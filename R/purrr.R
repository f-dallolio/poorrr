map <- function(.x, .f, ...) {
  .f <- as_function(.f, env = global_env())
  lapply(.x, .f, ...)
}
walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

map_lgl <- function(.x, .f, ...) {
  .map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  .map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  .map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  .map_mold(.x, .f, character(1), ...)
}
map_raw <- function(.x, .f, ...) {
  .map_mold(.x, .f, raw(1), ...)
}
.map_mold <- function(.x, .f, .mold, ...) {
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
imap <- function(.x, .f, ...) {
  map2(.x = .x, names(.x) %||% seq_along(.x), .f, ...)
}

pmap <- function(.l, .f, ...) {
  .f <- as.function(.f)
  args <- .args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}
.args_recycle <- function(args) {
  lengths <- vapply(args, length, integer(1))
  n <- max(lengths)
  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- lapply(args[to_recycle], function(x) rep.int(x, n))

  args
}


keep <- function(.x, .f, ...) {
  .x[where_if(.x, .f, ...)]
}
discard <- function(.x, .p, ...) {
  sel <- where_if(.x, .p, ...)
  .x[is.na(sel) | !sel]
}

map_if <- function(.x, .p, .f, ..., .else = NULL) {
  matches <- where_if(.x, .p)
  .x[matches] <- map(.x[matches], .f, ...)
  if(is.null(.else)) return(.x)
  matches <- !matches
  .x[matches] <- map(.x[matches], .f, ...)
  x
}
where_if <- function(.x, .p, ...) {
  if (is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    .p
  } else {
    .p <- as_function(.p, env = global_env())
    map_lgl(.x, .p, ...)
  }
}

map_at <- function(.x, .at, .f, ...) {
  map(.x[where_at(.x, .at)], .f, ...)
}

where_at <- function(.x, .at, ...) {
  if (is.call(.at) && call_name(.at)) .at <- as_function(.at)
  if (is.function(.at)) .at <- .at(names2(x), ...)

  if (is.numeric(.at) || is.logical(.at) || is.character(.at)) {
    if (is.character(.at)) .at <- intersect(.at, names2(x))

    loc <- vec_as_location(.at, length(.x), names2(.x))
    seq_along(.x) %in% loc
  } else {
    stop("`at` must be a numeric vector, character vector, or function.")
  }
}


compact <- function(.x) {
  Filter(length, .x)
}

transpose <- function(.l) {
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

  .l <- map(.l, as.list)

  map(fields, function(i) {
    map(.l, .subset2, i)
  })
}

every <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (is_false(.p(.x[[i]], ...))) return(FALSE)
  }
  TRUE
}
some <- function(.x, .p, ...) {
  .p <- as_function(.p, env = global_env())

  for (i in seq_along(.x)) {
    if (is_true(.p(.x[[i]], ...))) return(TRUE)
  }
  FALSE
}
none <- function(.x, .p, ...) {
  !some(.x, .p, ...)
}


negate <- function(.p) {
  .p <- as_function(.p, env = global_env())
  function(...) !.p(...)
}

reduce <- function(.x, .f, ..., .init) {
  f <- function(x, y) as_function(.f)(x, y, ...)
  Reduce(f, .x, init = .init)
}
reduce_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) as_function(.f)(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE)
}
accumulate <- function(.x, .f, ..., .init) {
  f <- function(x, y) as_function(.f)(x, y, ...)
  Reduce(f, .x, init = .init, accumulate = TRUE)
}
accumulate_right <- function(.x, .f, ..., .init) {
  f <- function(x, y) as_function(.f)(y, x, ...)
  Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}

detect <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(.x[[i]])
    }
  }
  NULL
}
detect_index <- function(.x, .f, ..., .right = FALSE, .p = is_true) {
  .p <- as_function(.p, env = global_env())
  .f <- as_function(.f, env = global_env())

  for (i in .index(.x, .right)) {
    if (.p(.f(.x[[i]], ...))) {
      return(i)
    }
  }
  0L
}
.index <- function(x, right = FALSE) {
  idx <- seq_along(x)
  if (right) {
    idx <- rev(idx)
  }
  idx
}

list_c <- function(x) {
  do.call(c, x)
}
purrr::list_rbind(list(as.matrix(letters), as.matrix(LETTERS)))
