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


ns_exports <- function (ns, sort = TRUE) {
  out <- getNamespaceExports(ns)
  if (sort) {
    sort(out)
  } else {
    out
  }
}
ns_privates <- function (ns, sort = TRUE) {
  out <- setdiff(names(asNamespace(ns)), getNamespaceExports(ns))
  if (sort) {
    sort(out)
  } else {
    out
  }
}
ns_names <- function(ns, sort = TRUE) {
  out <- names(asNamespace(ns))
  if (sort) {
    sort(out)
  } else {
    out
  }
}
ns_exports_has <- function (ns, name) {
  if (is_string(ns)) {
    ns <- rlang:::ns_env(ns)
  }
  if (identical(ns, ns_env("base"))) {
    exports <- baseenv()
  } else {
    exports <- ns$.__NAMESPACE__.$exports
  }
  !is.null(exports) && exists(name, envir = exports, inherits = FALSE)
}
ns_has <- function(ns, x)


ns_privates("purrr")
nms <- names(asNamespace("purrr"))
!nms %in% getNamespaceExports("purrr")
i <- rep("private", length(names(asNamespace("purrr"))))
split(names(asNamespace("purrr")), i)

f <- function(pkg) {
  pkg <- as.character(substitute(pkg))
  ns <- asNamespace(pkg)
  nms <- sort(names(ns))
  exprt <- getNamespaceExports(ns)
  e <- new.env()
  out <- list(.export = exprt,
              .private = nms[!nms %in% exprt],
              .ns = ns)
  structure(list2env(out, hash = FALSE),
            name = pkg,
            class = "ns_info")
}
print.ns_info <- function(x) {
  print(sprintf("<Namespace info: %s>", attr(x, "name")), quote = FALSE)
  invisible(x)
}


[nms %in% getNamespaceExports("purrr")] <- "export"
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

