# # x <- urls[[1]]
# # URL_parts <- function(x) {
# #   m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
# #   parts <- do.call(rbind,
# #                    lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
# #   colnames(parts) <- c("protocol","host","port","path")
# #   parts
# # }
# # URL_parts(x)
# #
# # do.call(rbind, lapply(strsplit(urls, "[:]//|/"), t))
# #
# # strsplit(dirname(urls), "[:]//") |>
# #   lapply(setNames, c("protocol", "host")) |>
# #   lapply(as.list) |>
# #   setNames(names(urls)) |>
# #   list_transpose() |>
# #   lapply(unlist) |>
# #   transform(
# #     host = sapply(strsplit(host, "/"), getElement, 1)
# #   )
# # |>
# #   apply(1, do.call, what = c)
# #   lapply(as.data.frame) |>
# #   unname()
# #   |>
# #   do.call(what = rbind) |>
# #   transform(
# #     host0 = gsub("^([^/]+)/", "\\1", host),
# #     dir = dirname(gsub("^([^/]+)/$", "", host)),
# #     file = basename(gsub("^([^/]+)/$", "", host))
# #   )
# #   transform()
# # basename(urls)
# #
# # pattern <- c("^(?<protocol>[[:alnum:]]+)[:]//",
# #              "(?<host>[[:alnum:]]*[.]?[[:alnum:]]+[.][[:alnum:]])/",
# #              "(?<dir>.*)/",
# #              "(?<file>[[:alnum:]]+)",
# #              "[.]*(?<ext>[[:alnum:]]*)$")
# # gregexpr(paste(pattern, collapse = ""),
# #         text = url,
# #         perl = TRUE)
#
# f <- function(url) {
#
#   parts <- c(protocol <- "^([^:]+)://",
#              host = "([^:])+[/].+$",
#              path = "(.*[.][[:alnum:]]+)$")
#   pattern = paste(parts, collapse = "")
#   proto <- data.frame(protocol = character(),
#                       host = character(),
#                       path = character())
#   strcapture(pattern, url, proto)
# }
#
# f(urls[[1]])
#
# `x <- "chr1:1-1000"
# pattern <- "(.*?):([[:digit:]]+)-([[:digit:]]+)"
# proto <- data.frame(chr=character(), start=integer(), end=integer())
# strcapture(pattern, x, proto)
