vec_is_list <- function(x) {
  is_bare_list <- !is.object(x) && is.list(x)
  is_bare_list || inherits(x, "list")
}
