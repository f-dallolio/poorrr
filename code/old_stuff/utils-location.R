# ith <- function(x, i, default = default_missing(x)) {
#   if (is_string(i)) {
#     i <- .str_as_pos2(i, names(x), default = NULL)
#   } else if (is_scalar_numeric(i)) (
#     i <- .num_as_pos2(i, length(x), default = NULL)
#   )
#   if (is.null(i)){
#     return(default)
#   }
#   .subset2(x, i)
# }
# row.names(as.matrix(mtcars))
# ith_slice <- function(x, i, default = default_missing(x)) {
#   if( is.data.frame(x) || is.matrix(x)) {
#     if(is_string(i)) {
#       i <- match(i, row.names(x), nomatch = 0)
#     } else {
#       i <- match(i, seq_len(nrow(x)), nomatch = 0)
#     }
#     return(x[i, ])
#   }
#
# }
#
# first <- function(x, default = default_missing(x)) {
#   ith(x, 1, default)
# }
# last <- function(x, default = default_missing(x)) {
#   ith(x, length(x), default)
# }
#
#
# vec_set_names <- function(x, names) {
#   if (is.data.frame(x)) {
#     attr(x, "row.names") <- names
#     return(x)
#   }
#   names(x) <- names
#   x
# }
#
# ith_slice <- function(x, i) {
#   if (is.data.frame(x)) {
#     if (is_string(i)) {
#       i <- match(i, attrs(x, "row.names"), 0)
#     }
#     out <- x[i, , drop = FALSE]
#     return(out)
#   }
#   .subset(x, 1)
# }
#
#
#
# ith_slice(x = mtcars, i = 0)
#
# ith(mtcars, -1)
#
# x <- as.list(letters)
#
# get_slice2(x = mtcars,i =  -1)
#
# get_slice <- function(x, i) {
#   stopifnot(is_string(i) || is_scalar_numeric(i))
#   if (is.data.frame(x)) {
#     nms <- attr(x, "row.names")
#     if (is.character(i)) {
#       i <- match(i, nms, nomatch = NA_integer_)
#     }
#     out <- lapply(x, )
#   }
#
# }
#
# get_slice2 <- function (x, i) {
#   if (is.data.frame(x)) {
#     xseq <- seq_len(nrow(x))
#     rnms <- attr(x, "row.names")
#     if (!identical(rnms, xseq) && is_string(i)) {
#       names(xseq) <- nms
#     }
#     x[, ]
#   }
# }
#
#   if (obj_is_list(x)) {
#     out <- .subset2(x, i)
#   } else {
#     out <- vec_slice(x, i)
#     out <- vec_set_names(out, NULL)
#   }
#   out
# }
#
#
# last(mtcars)
#
# .num_as_pos2 <- function(i, n, default = NA_integer_) {
#   if (length(n) > 1) {
#     n <- length(n)
#   }
#   n <- as.integer(n)
#   i <- as.integer(i)
#   if (i == 0 || i < -n || i > n) {
#     return(default)
#   }
#   if (i > 0) {
#     return(i)
#   }
#   n + i + 1
# }
#
# .str_as_pos2 <- function(i, n, default = NA_integer_, quiet = FALSE) {
#   if (!is.character(n)) {
#     n <- names(n)
#     if (is.null(n)) {
#       return(default)
#     }
#   }
#   out <- which(n == i)
#   nn <- length(out)
#   if (nn == 0) {
#     return(default)
#   }
#   if (nn == 1) {
#     return(out)
#   }
#   if (!quiet) {
#     warning("More than one match. Returning only the first.")
#   }
#   out[[1]]
# }
