# paths <- list.files(file.path(getwd(), "R"), full.names = TRUE)
# path <- paths[[1]]

parse_to_list  <- function(path) {
  x <- parse(path, keep.source = F)
  my_env <- new.env()
  eval(x, my_env)
  as.list(my_env, all.names = TRUE)
}
assn_names <- function(x) {
  if (is.expression(x)) {
    x <- as.list(x)
  }
  if (is.list(x)) {
    vapply(x, sys.function(), "")
  } else {
    deparse(x[[2]])
  }
}
assn_objs <- function(x) {
  if (is.expression(x)) {
    x <- as.list(x)
  }
  if (is.list(x)) {
    vapply(x, sys.function(), "")
  } else {
    deparse(x[[2]])
  }
}
#
#
# parseNamespaceFile(package = "purrr", package.lib = .libPaths()[[1]], mustExist = TRUE)
#
#
#
# list.files(.Library)
# list.files(.libPaths()[[1]], pattern = "purrr", full.names = T) |>
#   list.files()
#
# getNamespaceInfo(ns = "rlang")
# utils::str(allinfoNS("stats"))

