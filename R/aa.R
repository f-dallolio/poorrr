library(vctrs)

as_mold <- function (x = NULL, type = NULL) {
  if(is.null(type)) {
    if(is.null(x)) {
      stop("Must provide `.type` if `.x` is `NULL`.")
    }
    type <- typeof(x)
  } else {
    if(!is_string(type)) {
      stop("Argument `.type` must be a string.")
    }
  }
  is_mold(type)
}
is_mold <- function(type) {
  is.atomic(type) || type == "numeric" || length(type) > 1
}


can_coerce <- function (x, type) {
  actual <- typeof(x[[1]])
  if (is_mold(type)) {
    lengths <- unique(vapply(x, length, integer(1)))
    if (length(lengths) > 1 || !(lengths == length(type))) {
      return(FALSE)
    } else {
      type <- typeof(type)
    }
  }
  if (actual == "integer") {
    if (actual == "double" && type == "numeric") {
      return(TRUE)
    }
    if(type %in% c("integer", "double",  "numeric")) {
      return(TRUE)
    }
  }
  actual == type
}

list_types <- function(x) {
  vapply(x, typeof, character(1))
}
list_unique_types <- function (x, type = NULL, fun_p = NULL, ...) {
  x_type <- list_types(x)
  out <- length(unique(x_type)) == 1
  if(is.null(type) && is.null(fun_p)) {
    return(out)
  }
  if(!is.null(fun_p)) {
    matches <- vapply(x, as_predicate(fun_p), logical(1), ...)
  }
  if(!is.null(type)) {
    matches <- x_type %in% type && matches
  }
  out && all(matches)
}


x <- as.list(mtcars)
list_classes <- function(x, ..., .simplify = TRUE,  .as_df = FALSE) {
  out <- lapply(x, class)
  if (!.simplify && !.as_df) {
    return(out)
  }
  if(list_unique_types(out) == 1 && !.as_df) {
    return(unlist(out))
  }
  data.frame(i = rep(seq_along(out), lengths(out)),
             class = unlist(out),
             row.names = names(out))
}
list_classes0 <- function(x) {
  list_classes(x, .simplify = FALSE, .as_df = FALSE)
}
list_classes_df <- function(x) {
  list_classes(x, .simplify = TRUE, .as_df = TRUE)
}
can_simplify <- function(x, type = NULL) {
  if(any(lengths(x) > 1)) {
    return(FALSE)
  }
  if(all(vapply(x, is.atomic, logical(1)))) {
    types <- unique(list_types(x))
    n <- length(types)
    if( n > 1 && !all(c("double", "integer") %in% types)){
      return(FALSE)
    }
  }
  is.null(type) || can_coerce(x, type)
}



can_simplify <- function (x, type = NULL) {
  is_atomic <- vapply(x, is.atomic, logical(1))
  if (!all(is_atomic)) {
    return(FALSE)
  }
  mode <- unique(vapply(x, typeof, character(1)))
  if (length(mode) > 1 && !all(c("double", "integer") %in%
                               mode)) {
    return(FALSE)
  }
  is.null(type) || can_coerce(x, type)
}

can_simplify_all <- function(x, type = NULL) {
  if(!can_simplify(x)) {
    return(FALSE)
  }
  all(vapply(x, can_simplify, logical(1), type))
}

list_simplify <- function(x, strict = FALSE, ptype = NULL) {
  if(!can_simplify(x)) {
    if(strict) stop("x")
    if (is.null(ptype)) {
      return(x)
    }
    return(typeof(x) %in% ptype)
  }
  setNames(do.call(c, x), names(x))
}

list_simplify(as.list(letters))
