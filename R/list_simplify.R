#' Simplify a list to an atomic or S3 vector
#' @name list_simplify
NULL

#' @rdname list_simplify
#' @export
list_simplify <- function(x,
                          ...,
                          strict = TRUE,
                          # ptype = NULL,
                          error_arg = caller_arg(x),
                          error_call = caller_env()) {
  if (strict) {
    obj_check_list(x, arg = error_arg, call = error_call)
    list_check_all_vectors(x, arg = error_arg, call = error_call)
    list_check_all_size(x, 1, arg = error_arg, call = error_call)
  }
  list_simplify0(x, error_arg = error_arg, error_call = error_call)
}

list_simplify0 <- function(x,
                           ...,
                           error_arg = caller_arg(x),
                           error_call = caller_env()) {
  if (!obj_is_list(x) || !list_all_vectors(x) || !list_all_size(x, 1)) {
    return(x)
  }
  names <- vec_names(x)
  x <- vec_set_names(x, NULL)

  out <- do.call("c", x)
  vec_set_names(out, names)
}
