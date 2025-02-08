#' Flatten a list
#' @export
list_flatten <- function(x, ...) {
  # name_spec = "{outer}_{inner}",
  # name_repair = c("minimal", "unique", "check_unique", "universal")) {
  stopifnot(obj_is_list(x))
  stopifnot(...length() == 0)
  i <- vapply(x, obj_is_list, logical(1))
  x[i] <- lapply(x[i], unclass)
  x[!i] <- lapply(x[!i], list)
  unlist(x, recursive = FALSE)
}
