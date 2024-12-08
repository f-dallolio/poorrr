


pluck_raw <- function(index, .name = ".x") {
  stopifnot(is.symbol(name) || is_string(name))
  f <- function(x, y) call("[[", x, y)
  Reduce(f, index, as.symbol(name))
}



pluck_call(c(1,3), value = 3)

pluck_raw <- function(x, index, .default = NULL) {
  tryCatch(Reduce(f = `[[`, index, x),
           error = as.function(alist(e = , .default)))
}




x <- as.list(letters)
x[[length(x)]] <- setNames(seq_along(letters), LETTERS)
plucker(x, length(x), "A")


purrr:::pluck_raw(x, list(26, "A"))


reduce
