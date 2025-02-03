exports <- getNamespaceExports("purrr") |> setdiff(getNamespaceExports("poorrr")) |>
  sort() |>
  mget(asNamespace("purrr"), ifnotfound = list(NULL)) |>
  Filter(f = length) |>
  lapply(body) |>
  lapply(all.names) |>
  lapply(unique) |>
  sapply(function(x) !"lifecycle" %in% x) |>
  which() |>
  names() |>
  mget(asNamespace("purrr"))
names(exports)

names()
map(exports, as_call) |>
  imap(~ call("<-", as.symbol(.y), .x)) |>
  map_chr(as_string) |>
  cat(sep = "\n", file = "code/other_exports.R")

parse()

deparse(data.frame)


as_string <- function(x) {
  UseMethod("as_string")
}
as_string.call <- function(x) {
  paste0(deparse1(x), collapse = "\n")
}
as_string.default <- function(x) {
  paste0(deparse(x), collapse = "\n")
}
purrr::array_branch |>
  compact()

