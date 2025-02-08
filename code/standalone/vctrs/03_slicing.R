vec_size <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    length(x)
  }
}

vec_rep <- function(x, times) {
  i <- rep.int(seq_len(vec_size(x)), times)
  vec_slice(x, i)
}

vec_recycle_common <- function(xs, size = NULL) {
  sizes <- vapply(xs, vec_size, integer(1))
  n <- unique(sizes)
  if (length(n) == 1 && is.null(size)) {
    return(xs)
  }
  n <- setdiff(n, 1L)
  ns <- length(n)
  if (ns == 0) {
    if (is.null(size)) {
      return(xs)
    }
  } else if (ns == 1) {
    if (is.null(size)) {
      size <- n
    } else if (n != size) {
      stop("Inputs can't be recycled to `size`.", call. = FALSE)
    }
  } else {
    stop("Inputs can't be recycled to a common size.", call. = FALSE)
  }
  to_recycle <- sizes == 1L
  xs[to_recycle] <- lapply(xs[to_recycle], vec_rep, size)
  xs
}

vec_slice <- function(x, i) {
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.numeric(i) || is.character(i))
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    out <- x[i, 0, drop = FALSE]
    out[seq_along(x)] <- lapply(x, vec_slice, i)
    if (is.numeric(attr(x, "row.names"))) {
      row_names <- .set_row_names(nrow(out))
    } else {
      row_names <- attr(out, "row.names")
    }
    mtd <- .rlang_vctrs_s3_method("[", class(x))
    if (is_null(mtd) || identical(environment(mtd), asNamespace("base"))) {
      attrib <- attributes(x)
      attrib$row.names <- row_names
      attributes(out) <- attrib
    }
    return(out)
  }
  d <- vec_dims(x)
  if (d == 1) {
    if (is.object(x)) {
      out <- x[i]
    } else {
      out <- x[i, drop = FALSE]
    }
  } else if (d == 2) {
    out <- x[i, , drop = FALSE]
  } else {
    j <- rep(list(quote(expr = )), d - 1)
    out <- eval(as.call(list(quote(`[`), quote(x), quote(i),
      j,
      drop = FALSE
    )))
  }
  mtd <- .rlang_vctrs_s3_method("[", class(x))
  if (is_null(mtd) || identical(environment(mtd), asNamespace("base"))) {
    attrib <- attributes(x)
    attrib$names <- attr(out, "names")
    attrib$dim <- attr(out, "dim")
    attrib$dim.names <- attr(out, "dim.names")
    attributes(out) <- attrib
  }
  out
}

vec_dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}

vec_as_location <- function(i, n, names = NULL) {
  out <- seq_len(n)
  names(out) <- names
  if (is_logical(i, n = 1) && !length(out)) {
    return(out)
  }
  unname(out[i])
}

vec_init <- function(x, n = 1L) {
  vec_slice(x, rep_len(NA_integer_, n))
}

vec_assign <- function(x, i, value) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.logical(i)) {
    i <- which(i)
  }
  stopifnot(is.numeric(i) || is.character(i))
  value <- vec_recycle(value, vec_size(i))
  value <- vec_cast(value, to = x)
  d <- vec_dims(x)
  if (d == 1) {
    x[i] <- value
  } else if (d == 2) {
    x[i, ] <- value
  } else {
    stop("Can't slice-assign arrays.", call. = FALSE)
  }
  x
}

vec_recycle <- function(x, size) {
  if (is.null(x) || is.null(size)) {
    return(NULL)
  }
  n_x <- vec_size(x)
  if (n_x == size) {
    x
  } else if (size == 0L) {
    vec_slice(x, 0L)
  } else if (n_x == 1L) {
    vec_slice(x, rep(1L, size))
  } else {
    stop("Incompatible lengths: ", n_x, ", ", size, call. = FALSE)
  }
}
