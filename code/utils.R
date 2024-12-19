


global_env <- function() {
  globalenv()
}

names2 <- function(x) {
  nms <- names(x)
  if(is.null(nms)) return(setNames(x, rep("", length(x))))
  nms[is.na(nms)] <- ""
  nms
}
`names2<-` <- function(x, value) {
  value[is.na(value)] <- ""
  setNames(x, value)
}
set_names <- function(x, nm = x, ...) {
  names2(x) <- nm
  x
}


oxford_comma <- function (str, sep = ", ", final = "or") {
  n <- length(str)
  if (n < 2) return(str)
  head <- str[seq_len(n - 1)]
  last <- str[n]
  head <- paste(head, collapse = sep)
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}
oxford_comma_and <- function(str, sep = ", ") {
  oxford_comma(str, sep, final = "and")
}

rev_index0 <- function(i, n, ...) {
  rm <- i < -n
  if(any(rm)) {
    pl <- if(length(which(rm))) "s" else ""
    msg <- sprintf("Element%s %s are smaller than `n`",
                   pl,
                   oxford_comma_and(which(rm)))
    if(.strict) stop(msg, ".")
    if(.verbose) warning(msg, " and are being ignored.")
  }
  i <- i[!rm]
  if(length(i) == 0) return(integer())
  if(length(i) > 1) {
    rev <- i < 0
    i[rev] <- n + 1 + i[rev]
    match(i, seq_len(n))
  } else {
    n + 1 + i
  }
}

num_as_location <- function(i, n, ..., rev = FALSE) {
  if(is.logical(i)) i <- which(i)
  stopifnot(is.numeric(i))
  if(rev) return(rev_index(i, n))
  out <- seq_len(n)
  # Special-case recycling to size 0
  if (is.logical(i) && length(i) == 1 && !length(out)) return(out)
  unname(out[i])
}




vec_size <- function(x) {
  if(is.data.frame(x)) return(NROW(x))
  length(x)
}

vec_slice <- function(x, i) {
  x[vec_as_location(i, length(x), names = names2(x))]
}

`vec_slice<-` <- function(x, i, value) {
  x[vec_as_location(i, length(x), names = names2(x))] <- value
  x
}
set_slice <- function(x, i, value) {
  `vec_slice<-`(x, i, value)
}

vec_slice2 <- function(x, i) {
  stopifnot(length(i) == 1)
  x[vec_as_location(i, length(x), names = names2(x))]
}
`vec_slice2<-` <- function(x, i, value) {
  if(is.logical(i)){
    stopifnot(length(i) == length(x))
    i <- which(i)
  }
  stopifnot(length(i) == 1)
  x[[vec_as_location(i, length(x), names = names2(x))]] <- value
  x
}
set_slice2 <- function(x, i, value) {
  `vec_slice2<-`(x, i, value)
}

assign_in <- function (x, where, value) {
  n <- length(where)
  if (n == 0) {
    stop("`where` must contain at least one element.")
  } else if (n > 1) {
    i <- where[[1]]
    if(is.numeric(i))
    old <- x[][[1]] %||% list()
    if (!inherits(x, "rlang_zap") || !identical(old, list())) {
      value <- assign_in(old, where[-1], value)
    }
  }
  if (inherits(x, "rlang_zap")) {
    x[where[[1]]] <- list(NULL)
  } else {
    if (is.null(value)) {
      x[where[[1]]] <- list(NULL)
    } else {
      x[where[[1]]][[1]] <- value
    }
  }
  x
}






is_scalar <- function(x) {
  is_vector(x, n = 1)
}



zap <- function() {
  structure(list(), class = "rlang_zap")
}



has_zap <- function(x, negate = FALSE) {
  out <- any(vapply(x, is_zap, TRUE))
  if(negate) return(!out)
  out
}

unstructure <- function(x) {
  attributes(x) <- NULL
  x
}
