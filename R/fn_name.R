#' @export
fn_name <- function(fn) {
  switch(typeof(fn),
         "builtin" = ,
         "special" = .fn_name_primitive(fn),
         "closure" = .fn_name_closure(fn),
         stop("argument is not a function")
  )
}

.fn_name_closure <- function(fn) {
  env <- environment(fn)
  fns <- mget(names(env), env, mode = "function", inherits = FALSE, ifnotfound = list(NULL))
  for (i in seq_along(fns)) {
    if (identical(fn, fns[[i]])) {
      return(names(fns)[[i]])
    }
  }
  NULL
}
.fn_name_primitive <- function(fn) {
  str2lang(deparse(fn))[[2]]
}
