data_frame <- function(...) {
  new_data_frame(df_list(...), .class = "tbl")
}

new_data_frame <- function(.x = list(), ..., .size = NULL, .class = NULL) {
  n_cols <- length(.x)
  if (n_cols != 0 && is.null(names(.x))) {
    stop("Columns must be named.", call. = FALSE)
  }
  if (is.null(.size)) {
    if (n_cols == 0) {
      .size <- 0
    } else {
      .size <- vec_size(.x[[1]])
    }
  }
  structure(.x,
    class = c(.class, "data.frame"), row.names = .set_row_names(.size),
    ...
  )
}

df_list <- function(..., .size = NULL) {
  vec_recycle_common(list(...), size = .size)
}
