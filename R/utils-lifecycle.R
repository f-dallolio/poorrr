#
# # # pkg <- "poorrr"
# x <- as.list(asNamespace("purrr"), all.names = TRUE)
# nms <- Filter(is.function, x) |>
#   Filter(f = Negate(is_lifecycle)) |>
#   names() |>
#   intersect(getNamespaceExports("purrr")) |>
#   setdiff(names(asNamespace("poorrr"))) |>
#   sort()
# nms
# |>
#   which() |>
#   names()
#
# x <- x[x$status == "" & x$export & !(x$obj %in% names(asNamespace("poorrr"))), ]
# !x$obj %in%
# get("flatten_df", envir = asNamespace("purrr"))
# purrr:::flatten_df
# xx <- names(asNamespace("poorrr"))[names(asNamespace("poorrr")) %in% getNamespaceExports("purrr")]
# obj2 <- data.frame(obj = getNamespaceExports("purrr") |> setdiff(xx))
# out <- merge(pkg_objs("purrr"), obj2)
# out[out$status == "", ]


pkg_objs <- function(pkg) {
  # requireNamespace(as.character(substitute(pkg)))
  ns <- asNamespace(pkg)
  objs <- unlist(mget(names(ns), ns, ifnotfound = list(NULL), inherits = FALSE))
  x <- data.frame(
    obj = names(objs),
    export = names(objs) %in% getNamespaceExports(pkg),
    mode = map_chr(objs, mode),
    type = map_chr(objs, mode),
    class = map_chr(objs, \(x) class(x)[[1]])
  )
  status <- pkg_lifecycle_statuses(pkg)[, -1]
  row.names(status) <- NULL
  names(status) <- c("obj", "status")
  out <- merge(x, status, all.x = TRUE)
  out$status[is.na(out$status)] <- ""
  out[c("obj", "status", "export", "mode", "type", "class")]
}


pkg_lifecycle_statuses <- function(package, which = c("superseded", "deprecated", "questioning", "defunct", "experimental", "soft-deprecated", "retired")) {
  check_installed("vctrs")
  which <- match.arg(which, several.ok = TRUE)
  stopifnot(is_string(package))
  db <- tools::Rd_db(package)
  lc <- db_lifecycle(db)
  funs <- db_function(db)
  res <- mapply(function(lc, f) data.frame(fun = f, lifecycle = rep(lc, length(f)), stringsAsFactors = FALSE),
    lc,
    funs,
    SIMPLIFY = FALSE
  )
  res <- do.call("rbind", res)
  if (!NA %in% which) {
    res <- res[!is.na(res$lifecycle), ]
  }
  res <- res[nzchar(res$fun), ]
  res <- res[grep("[\\\\]method\\{", res$fun, invert = TRUE), ]
  res <- res[res$lifecycle %in% which, ]
  if (nrow(res) == 0) {
    return(data.frame(
      package = character(), fun = character(),
      lifecycle = character()
    ))
  }
  res$package <- package
  res[c("package", "fun", "lifecycle")]
}

db_lifecycle <- function(db) {
  lifecycle_patterns <- paste0(
    "(?:",
    paste(
      collapse = "|",
      c(
        "lifecycle::badge\\([\\\\]\"",
        "rlang:::lifecycle\\([\\\\]\"",
        "list\\(\"lifecycle-", "https://www.tidyverse.org/lifecycle/#"
      )
    ),
    ")([\\w-]+)"
  )
  desc <- lapply(
    db, asNamespace("tools")$.Rd_get_metadata,
    "description"
  )
  lapply(desc, function(x) {
    res <- regexpr(lifecycle_patterns, x, perl = TRUE)
    starts <- attr(res, "capture.start")
    ends <- starts + attr(res, "capture.length") - 1
    substring(x, starts, ends)
  })
}
db_function <- function(db) {
  usage <- lapply(db, asNamespace("tools")$.Rd_get_section, "usage")
  lapply(usage, get_usage_function_names)
}

get_usage_function_names <- function(x) {
  if (!length(x)) {
    character(1)
  } else {
    res <- asNamespace("tools")$.parse_usage_as_much_as_possible(x)
    vapply(res, function(x) {
      if (is.call(x)) {
        as.character(x[[1]])
      } else {
        character(1)
      }
    }, character(1))
  }
}
