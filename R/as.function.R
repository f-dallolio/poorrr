# #' @export
# as.function.formula <- function(x) {
#   stopifnot(length(x) == 2)
#   out <- eval(call("function",
#                    as.pairlist(alist(... = , .x = ..1, .y = ..2, . = ..1)),
#                    x[[2]]))
#   environment(out) <- environment(x)
#   class(out) <- c("rlang_lambda_function", "function")
#   out
# }
# #' @export
# as.function.call <- function(x) {
#   stopifnot(is_call_simple(x))
#   if (is_call(x[[1]], c("::", ":::"))) {
#     fn <- eval(x[[1]])
#   } else {
#     fn <- eval(x[[1]], envir = parent.frame())
#   }
#   stopifnot(is.function(fn))
#   fn
# }
# #' @export
# as.function.name <- function(x) {
#   fn <- eval(x)
#   stopifnot(is.function(fn))
#   fn
# }
