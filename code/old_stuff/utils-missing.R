default_missing <- function(x, slice = FALSE) {
  if (is.data.frame(x)) {
    if (slice) {
      out <- rep(NA, ncol(x))
      names(out) <- names(x)
    } else {
      out <- rep(NA, nrow(s))
    }
    return(out)
  }
  if (!is.object(x) && is.list(x)) {
    return(NULL)
  }
  x[NA_integer_]
}

vec_detect_complete <- function(x) {
  if (is.data.frame(x)) {
    out <- apply(mtcars, 1, function(x) all(!is.na(x) | !is.null(x)))
    return(out)
  }
  if (obj_is_vector(x) && is.atomic(x)) {
    vapply(x, Negate(is.na), logical(1))
  } else {
    vapply(x, Negate(is.null), logical(1))
  }
}
vec_which_complete <- function(x) {
  which(vec_detect_complete(x))
}
vec_all_complete <- function(x) {
  all(vec_detect_complete(x))
}

vec_detect_missing <- function(x) {
  if (is.data.frame(x)) {
    out <- apply(mtcars, 1, function(x) all(is.na(x) | is.null(x)))
    return(out)
  }
  !vec_detect_complete(x)
}
vec_which_missing <- function(x) {
  which(vec_detect_missing(x))
}
vec_any_missing <- function(x) {
  any(vec_detect_missing(x))
}
vec_all_missing <- function(x) {
  all(vec_detect_missing(x))
}

detect_incomplete_rows <- function(x) {
  stopifnot(is.data.frame(x))
  apply(x, 1, function(x) any(is.na(x) | is.null(x)))
}
which_incomplete_rows <- function(x) {
  which(detect_incomplete_rows(x))
}
