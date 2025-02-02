getNamespaceExports("purrr") |> setdiff(getNamespaceExports("poorrr")) |>
  sort() |>
  mget(asNamespace("purrr"), ifnotfound = list(NULL)) |>
  Filter(f = length) |>
  lapply(body) |>
  lapply(all.names) |>
  lapply(unique) |>
  sapply(function(x) !"lifecycle" %in% x) |>
  which() |>
  names()
