parse_to_list <- function(con) {
  if (length(con) > 1) {
    return(lapply(con, sys.function()))
  }
  parsed_to_list(parse(con, keep.source = FALSE))
}

parsed_to_list <- function(expr) {
  if (obj_is_list(expr) || is.expression(expr)) {
    out <- lapply(expr, sys.function())
    return(unlist(out))
  }
  e <- new.env()
  eval(expr, e)
  out <- as.list(e, all.names = TRUE)
  i <- vapply(out, typeof, "") == "closure" | vapply(
    out, inherits,
    FALSE, "formula"
  )
  out[i] <- lapply(out[i], `environment<-`, globalenv())
  out
}

parse_source <- function(path) {
  if (length(path) > 1) {
    names(path) <- str_remove_all(basename(path), "[.].+$")
    out <- lapply(path, parse_source)
    return(out)
  }
  get_assign_call(parse(path, keep.source = FALSE))
}

is_call_assign <- function(x) {
  is_call(x, "<-")
}

get_assign_obj <- function(x) {
  out <- x[[3]]
  if (!is_call_assign(out)) {
    return(out)
  }
  get_assign_obj(out)
}

get_assign_names <- function(x, name = NULL, ...) {
  name <- c(name, deparse(x[[2]]))
  if (!is_call_assign(x[[3]])) {
    return(name)
  }
  get_assign_names(x[[3]], name = name)
}

get_assign_call <- function(x) {
  if (is.list(x) || is.expression(x)) {
    out <- lapply(x, get_assign_call)
    return(unlist(out))
  }
  if (!is_call_assign(x)) {
    return(x)
  }
  nms <- get_assign_names(x)
  out <- rep(list(get_assign_obj(x)), length(nms))
  names(out) <- nms
  lapply(out, call_as_fn)
}

is_call_fml <- function(x) {
  is_call(x, "~")
}

call_as_fml <- function(x, env = topenv()) {
  if (!is_call_fml(x)) {
    return(x)
  }
  as.formula(x, env = env)
}

is_call_fn <- function(x) {
  is_call(x, "function")
}

call_as_fn <- function(x) {
  if (!is_call_fn(x)) {
    return(x)
  }
  out <- unlist(call_args(x))
  n <- length(out)
  if (!is_call(out[[n]], "{")) {
    out <- c(out[-n], call("{", out[[n]]))
  }
  as.function(out, topenv())
}
