# # repo_spec
# #
# # fn <- function(x, tag = "export") {
# #   if(is.list(x)) {
# #     unlist(lapply(x, sys.function(), tag))
# #   } else {
# #     if(!is.call(x)) return(NULL)
# #     if(deparse(x[[1]]) == tag) {
# #       as.character(x[[2]])
# #     } else {
# #       NULL
# #     }
# #   }
# # }
# # map(keep(x, is_call, "export"), .subset2,)
#
# ns_exports <- function(repo_spec) {
#   endpoint <- sprintf("/repos/%s/contents/NAMESPACE", repo_spec)
#   lines <- readLines(gh::gh(endpoint)$download_url)
#   x <- grep("^export\\(.+\\)", lines, value = TRUE)
#   x <- as.list(str2expression(x))
#   as.character(lapply(x, .subset2, 2))
# }
# #
# #
# #
# # f <- function(x) {
# #   if(is.list(x)) {
# #     unlist(lapply(x, sys.function()))
# #   } else {
# #     if(!is.call(x) || deparse(x[[1]]) != "export") return(NULL)
# #     as.character(x[[2]])
# #   }
# # }
# # x <- as.list(parse(ns_url, keep.source = TRUE))
# #
# # url <- (ns_url)
# # vapply(x,
# #        ,
# #        logical(1))
# #
# # x <- grep(export_pattern, , value = TRUE)
# # x <- gsub(export_pattern, "\\1", x)
# # gsub("\"", "", x)
#

repo_urls <- function (repo_spec) {
  ns_endpoint <- sprintf("/repos/%s/contents/NAMESPACE", repo_spec)
  ns_url <- gh::gh(ns_endpoint)$download_url

  exports <- as.list(parse(ns_url, keep.source = FALSE))

  nms_exports <- as.character(lapply(exports, .subset2, 1))
  names(exports) <- nms_exports

  id_exports <- nms_exports == "export"
lapply(exports[id_exports], .subset2, 2)
#
#
#
#   r_endpoint <- sprintf("/repos/%s/contents/R", repo_spec)
#
#
#   r_urls <- vapply(gh::gh(r_endpoint), .subset2, "", "download_url")
#   r_nms <- gsub("[.].+$", "", basename(r_urls))
#   rm_patterns <- "^reexport|^superseded|^deprec|-package$|^package-"
#   r_urls <- r_urls[grep(rm_patterns, r_nms, invert = TRUE)]
#
#   structure(
#     r_urls,
#     repo = repo_spec,
#     ns_url = ns_url,
#     class = c("repo_urls")
#   )
#
# }
#
# parse_to_list <- function(url, env = parent.frame()) {
#   if(length(url) > 1) {
#     names(url) <- gsub("[.].+$", "", basename(url))
#     return(lapply(url, ff, env))
#   }
#   my_env <- new.env()
#   eval(parse(url, keep.source = FALSE), my_env)
#   out <- as.list(my_env, all.names = TRUE)
#   id_fn <- vapply(out, typeof, "") == "closure"
#   id_fml <- vapply(out, inherits, logical(1), "formula")
#   out[id_fn | id_fml] <- lapply(out[id_fn | id_fml], `environment<-`, env)
#   out
# }
#
# repo_spec = "tidyverse/purrr"
# parse_repo <- function(repo_spec, ..., env = parent.frame()) {
#   url <- repo_urls(repo_spec)
#   x <- parse_to_list(url, env = env)
#   x_env <- list2env(unlist(unname(x)))
#   exports <- ns_exports(repo_spec)
#
#   out <- data.frame(repo_file = rep(names(x), lengths(x)),
#                     name = unlist(lapply(unname(x), names)),
#                     row.names = NULL)
#   out$export <- out$name %in% exports
#   structure(out,
#             repo = repo_spec,
#             objs = x_env,
#             class = c("xxx", class(out)))
# }
#
# xx <- parse_repo(repo_spec)
# xx
#
# paths <- list.files("R", full.names = TRUE)
# paths <- paths[!grepl("^package-|-package[.][Rr]$", basename(paths))]
# paths <- file.path(getwd(), paths)
#
#
#
# nms <- parse_to_list(paths)
# loc_fns <- data.frame(file = rep(names(nms), lengths(nms)),
#                       name = unlist(lapply(unname(nms), names)),
#                       row.names = NULL)
#
# dir.create("test")
# loc_fns$path <- file.path(getwd(), "test", paste0(loc_fns$file, ".R"))
#
#
#
#
# loc_fns <- split(loc_fns, ifelse(loc_fns$name %in% xx$name, "in_repo", "out_repo"))
#
# do.call(rbind, unname(loc_fns))
#
# merge(loc_fns$in_repo, xx)
#
# ?dplyr:::left_join
# ff(repo_urls(repo_spec))
#
# merge
#
# f <- function(url) {
#
#   if (length(url) > 1) {
#     names(url) <- gsub("[.].+$", "",basename(url))
#     out <- lapply(url, sys.function())
#     return(out)
#   }
#
#   x <- as.list(parse(url, keep.source = FALSE))
#   id <- vapply(x, inherits, logical(1), "<-")
#   as.character(lapply(x[id], .subset2, 2))
# }
# f(repo_urls(repo_spec))
#
#
#
#
#
# my_env <- new.env()
# eval(x, my_env)
# out <- as.list(my_env, all.names = TRUE)
#
#
#
# get_assn_fns <- function(x) {
#   if(inherits(x, "<-")) {
#     obj <- eval(x[[3]])
#     name <- deparse(x[[2]])
#   }
#
# }
#
# repo_urls <- function(repo_spec = "tidyverse/purrr", folder = "R") {
#   input <- gh::gh(sprintf("/repos/%s/contents/%s", repo_spec, folder))
#   vapply(input, getElement, "", "download_url")
# }
#
# repo_ns_exports <- function(repo_spec = "tidyverse/purrr") {
#   input <- gh::gh(sprintf("/repos/%s/contents/NAMESPACE", repo_spec))
#   x <- grep("^export", readLines(input$download_url), value = TRUE)
#   x <- gsub("^export\\((.+)\\).*$", "\\1", x)
#   vapply(x, \(x) as.character(str2lang(x)), "", USE.NAMES = FALSE)
# }
#
# assn_names <- function(url) {
#   if(length(url) > 1) {
#     out <- lapply(unname(url), sys.function())
#     out <- do.call(rbind, out)
#     return(out)
#   }
#   src <- parse(url, keep.source = TRUE)
#   is_assign <- vapply(as.list(src), inherits, logical(1), "<-")
#   nms <- lapply(as.list(src)[is_assign], getElement, 2)
#   if(length(nms) == 0) return(NULL)
#
#   file <- gsub("[.].+$", "", basename(url))
#   if(grepl("^reexport|^superseded|^deprec|-package$|^package-", file)) {
#     return(NULL)
#   }
#   name <- vapply(nms, deparse, "")
#   data.frame(file = file,
#              name_pos = seq_along(name),
#              name = name,
#              row.names = NULL)
# }
#
# repo_assn_names <- function(..., repo_spec = NULL) {
#   if(is.null(repo_spec)) {
#     url <- as.character(list(...))
#     assn_names(url)
#   } else {
#     out <- assn_names(repo_urls(repo_spec))
#     exports <- repo_ns_exports(repo_spec)
#     out$export = out$name %in% exports
#     out
#   }
# }
# repo_spec = "tidyverse/purrr"
# repo_assn_names(repo_spec = repo_spec)
