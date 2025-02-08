list_simplify <- function(x,
                          strict = TRUE,
                          ptype = NULL,
                          error_arg = caller_arg(x),
                          error_call = caller_env()) {
  obj_check_list(x, arg = error_arg, call = error_call)

  if (strict) {
    list_check_all_vectors(x, arg = error_arg, call = error_call)
    list_check_all_size(x, 1, arg = error_arg, call = error_call)
  } else {
    can_simplify <- list_all_vectors(x) && list_all_size(x, 1)
    if (!can_simplify) {
      return(x)
    }
  }

  names <- vec_names(x)
  x <- vec_set_names(x, NULL)

  out <- do.call("c", x)
  vec_set_names(out, names)
}
