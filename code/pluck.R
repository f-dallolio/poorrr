#
#
#
# # pluck_raw <- function(.x, index, .name = ".x") {
# #   stopifnot(is.symbol(name) || is_string(name))
# #   f <- function(x, y) call("[[", x, y)
# #   Reduce(f, index, as.symbol(name))
# # }
#
#
# pluck_raw <- function(.x, index, .default = NULL) {
#   tryCatch(Reduce(f = vec_slice2, index, x),
#            error = as.function(alist(e = , .default)))
# }
#
# x <- list(3,2,1)
# f <- function(.x, index, value, default = NULL) {
#   .call <- Reduce(as_function(~ call("[[", .x, .y)), append(quote(.x), as.list(index)))
#   do.call(set_slice2, c(.call, value))
# }
#
#
#
#
# f(letters, list(26, "A"), value = 100)
#
# Reduce(\(x, y) call("[[", x, y), 10:1)
#
# call("[[",call("[[", 1, 2), 3)
