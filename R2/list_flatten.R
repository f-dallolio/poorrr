#' Flatten a list
#' @name list_flatten
NULL

#' @rdname list_flatten
#' @export
list_flatten <- function(x, ..., name_spec = "{outer}_{inner}", name_repair = c(
           "minimal",
           "unique", "check_unique", "universal"
         )) {
  obj_check_list(x)
  stopifnot(...length() == 0)
  stopifnot(is_string(name_spec))
  x <- tryCatch(vec_proxy(x), error = function(e) x)
  x <- map_if(x, obj_is_list, unclass, .else = list)
  if (!is.null(name_spec) && !is.na(name_spec) && name_spec !=
    "" && !is.null(names(x))) {
    x <- imap(x, as_name_spec, name_spec, match.arg(name_repair))
  }
  do.call("c", x)
}

flatten_spliced <- function(.x) {
  .x <- map_if(.x, is_spliced, .subset2, 1)
  list_flatten(.x, name_spec = "{inner}")
}
is_unnamed <- function(x) {
  nms <- names(x)
  is.null(nms) || all(nms == "" | is.na(nms))
}
list_all_unnamed <- function(x) {
  stopifnot(obj_is_list(x))
  is_unnamed(x) && all(vapply(x, is_unnamed, logical(1)))
}
as_name_spec <- function(x, name_spec = "{outer}_{inner}", name_repair = c(
           "minimal",
           "unique", "check_unique", "universal"
         )) {
  if (is.null(outer) || !grepl("\\{outer}", name_spec) || is.na(outer) ||
    outer == "") {
    return(x)
  }
  name_spec <- gsub("{outer}", outer, name_spec, fixed = TRUE)
  name_spec <- gsub("{inner}", "%s", name_spec, fixed = TRUE)
  inner <- names2(x)
  i <- inner == ""
  inner[i] <- outer
  inner[!i] <- sprintf(name_spec, inner)
  names2(x) <- vec_as_names(inner, match.arg(name_repair))
  x
}


