deframe <- function(x) {
  if (length(x) == 1) {
    return(x[[1]])
  } else if (length(x) != 2) {
    warn("`x` must be a one- or two-column data frame in `deframe()`.")
  }
  value <- x[[2L]]
  name <- x[[1L]]
  set_names(value, format(name))
}

enframe <- function(
    x, name = "name", value = "value", ..., arg = caller_arg(x),
    call = caller_env()) {
  if (is.null(value)) {
    abort("Argument `value` cannot be `NULL`.")
  }
  if (is.null(x)) {
    x <- logical()
  }
  if (!obj_is_vector(x) || is.data.frame(x)) {
    msg <- paste0(
      "The `x` argument to `enframe()` must be a vector, not ",
      class(x)[[1]], "."
    )
    abort(msg)
  }
  if (is.null(name)) {
    df <- list(vectbl_set_names(x, names = NULL))
  } else if (is.null(vec_names(x))) {
    df <- list(seq_len(vec_size(x)), x)
  } else {
    df <- list(vec_names2(x), vectbl_set_names(x))
  }
  names(df) <- c(name, value)
  new_data_frame(df, .size = vec_size(x), .class = "tbl")
}