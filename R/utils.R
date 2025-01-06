make_function <- function (args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  stopifnot(!is.null(names(args)), all(names(args) != ""), is.language(body))
  eval(call("function", args, body), env)
}

make_call <- function (f, ..., .args = list()) {
  if (is.character(f)) f <- as.name(f)
  as.call(c(f, ..., .args))
}


as_predicate <- function (.fn, ...) {
  .fn <- as_function(.fn)
  function(...) {
    out <- .fn(...)
    if (!is_bool(out)) {
      abort(sprintf("Predicate functions must return a single `TRUE` or `FALSE`, not %s",
                    as_predicate_friendly_type_of(out)))
    }
    out
  }
}


oxford_comma <- function (x, sep = ", ", final = "or") {
  n <- length(x)
  if (n < 2) {
    return(x)
  }
  head <- x[seq_len(n - 1)]
  last <- x[n]
  head <- paste(head, collapse = sep)
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}
oxford_comma_and <- function (x) {
  oxford_comma(x, sep = ", ", final = "and")
}
oxford_comma_or <- function (x) {
  oxford_comma(x, sep = ", ", final = "or")
}

`list_slice2<-` <- function (x, i, value) {
  if (is.null(value)) {
    x[i] <- list(NULL)
  } else {
    x[[i]] <- value
  }
  x
}

unstructure <- function (x) {
  attributes(x) <- NULL
  x
}

path_rm_ext <- function(x) {
  gsub("[.]*[[:alnum:]]*$", "", x)
}
path_dir <- function(x) {
  dirname(x)
}
path_file <- function(x, rm_ext = FALSE) {
  if(rm_ext) {
    path_rm_ext(basename(x))
  } else {
    basename(x)
  }
}

is_string <- function(x) {
  is.character(x) && length(x) == 1
}
is_bool <- function (x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

