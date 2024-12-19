# # .pluck_raw_i <- function(x, index, .default) {
# #   if (length(index) > 1) {
# #     pluck_raw
# #   }
# #
# #   index_num <- 1 + mget("index_num", inherits = FALSE, ifnotfound = 0)
# #   .get_ith(.x, index), index_num = index_num)
# #
# #
# # }
#
# pluck_raw <- function(.x, index, .default) {
#   if(!is.list(index)) index <- as.list(index)
#   out <- Reduce(.get_ith, index, init = .x)
#   if(is.null(out)) return(.default)
#   out
# }
#
# pluck <- function(.x, ..., .default = NULL) {
#   index <- rlang::list2(...)
#   out <- Reduce(.get_ith, index, init = .x)
#   if(is.null(out)) return(.default
#   out
# }
#
# pluck_exists <- function (.x, ...) {
#   !is_zap(pluck(.x, rlang::list2(...), .default = zap()))
# }
#
# .chuck <- function(.x, .i, .index_num = NULL) {
#
# }
# chuck <- function(.x, ...) {
#
#   out <- pluck(.x, ..., .default = zap())
#   if (!is_zap(out)) return(out)
#
# }
#
# `pluck<-` <- function(x, ..., value) {
#   assign_in(x, list(...), value)
# }
#
# assign_in <- function (.x, where, value) {
#   n <- length(where)
#   if (n == 0) {
#     stop("`where` must contain at least one element.")
#   } else if (n > 1) {
#     old <- pluck(.x, where[[1]], .default = list())
#     if (!inherits(value, "rlang_zap") || !identical(old, list())) {
#       value <- assign_in(old, where[-1], value)
#     }
#   }
#   where <- .subset2(where, 1)
#   nx <- length(.x)
#   if(is.numeric(where) && where < 0 && where > -nx) {
#     where <- .as_index(where, nx, names = NULL)
#   }
#   if (inherits(value, "rlang_zap")) {
#     .x[[where]] <- NULL
#   } else {
#     list_slice2(.x, where) <- value
#   }
#   .x
# }
#
#
#
#
# .as_index <- function(i, n, names) {
#   if (length(i) > 1) {
#     out <- vapply(i, .as_index, numeric(1), n, names)
#     return(out)
#   }
#   if(is.character(i)) {
#     i <- match(i, names, nomatch = 0)
#   }
#   if(i > 0 && i < n) return(i)
#   if(i < 0 && i > -n) return(n + i + 1)
#   NA_real_
# }
# get_nth <- function (x, n, order_by = NULL) {
#   if (length(n) != 1 || !is.numeric(n))
#     stop("`n` must be a single integer.")
#   n <- trunc(n)
#   if (n == 0 || n > length(x) || n < -length(x)) {
#     if(is.null(default)) {
#       if(is.data.frame(x)) return(rep(NA, NROW(x)))
#       if (!is.object(x) && is.list(x)) return(NULL)
#       x[NA_real_]
#     }
#   }
#   if (n < 0)
#     n <- length(x) + n + 1
#   if (is.null(order_by))
#     x[[n]]
#   else x[[order(order_by)[[n]]]]
# }
#
# nth(list(1), 2)
#
# .get_ith <- function(x, i) {
#   stopifnot(length(i) == 1)
#   i <- .as_index(i, length(x), names(x))
#   if(is.list(x)) x <- as.list(x)
#   .subset(x, i)[[1]]
# }
#
# nth(letters, 30)
#
.subscript_rev <- function(i, n, .negative = FALSE) {
  stopifnot(length(i) > 0)
  if(is.list(i)) {
    id_num <- vapply(i, is.numeric, logical(1))
    i[id_num] <- vapply(i[id_num], .subscript_rev, numeric(1), n, .negative)
    return(i)
  }
  out <- ifelse(i < 0, n + i + 1, i)
  if(.negative) {
    return(out)
  }
  ifelse(out < 0, i[NA_real_], out)
}


.num_as_subsript2 <- function (i, n, .default,  .rev = TRUE, .strict = FALSE) {
  stopifnot(length(i) == 1)
  if(!is.list(i)) {
    i <- .subset2(i, )
  }
  if(.strict) {
    .rev <- FALSE
  }
  if(.rev) {
    i <- .subscript_rev(i, n, .default, .negative = .strict)
  }
  if (.strict && i > n && i < 0) {
    return(i[NA_real_])
  }
  i
}
.num_as_subscript2 <- function (i, n = NULL, .default = NULL, .strict_side = c("none", "lower", "upper", "both")) {
  stopifnot(length(i) == 1)
  if(is.null(n) && i > 0 && i < n) {
    return(i)
  }
  if (i < -n) {
    return(.default)
  }
  .strict_side <- match.arg(.strict_side)
  if (.strict_side == "both") {

  }
  is_lower <- i < 0 && i > -n
  is_upper <- i > n
  if (.strict_side == "both") {
    if(is_lower || is_upper) return(.default)
    return(i)
  }
  if (.strict_side == "lower" && is_lower) {
    return(.default)
  } else {
    return(n + i + 1)
  }
  if (.strict_side == "upper" && is_upper) {
    return(.default)
  }
  i
}

maybe_record <- function (x) {
  if (inherits(x, "vctrs_rcrd")) return(TRUE)
  if(is.data.frame(x)) return(FALSE)
  x <- unclass(x)
  list_same_sizes(x) && is.list(x)
}

vec_size_common <- function(x, size = NULL) {
  sizes <- vapply(x, rlang:::vec_size, integer(1))
  n <- unique(sizes)
  len <- length(n)
  if (any(n) == 0) {
    return(0)
  }
  if (len > 2 || !all(n %in% c(1, size %||% max(n)))) {
    abort_size_common(n)
  }
  n
}

list_sizes <- function(x) {
  vapply(x, vec_size, numeric(1))
}

list_same_sizes <- function(x) {
  sizes <- vapply(x, rlang:::vec_size, integer(1))
  length(unique(sizes)) == 1
}

list_all_sizes <- function (x, size = NULL) {
  if(is.null(size)) {
    return(list_same_sizes)
  }
  list_sizes(x) == size
}

