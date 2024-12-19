repo <- "tidyverse/purrr"
path <- "R"

# # fn <- function (repo, path = "R") {
#   input <- gh::gh(sprintf("/repos/%s/contents/%s", repo, path))
#   urls <- vapply(input, getElement, "", "download_url")
#   nms <- gsub(".[Rr]$", "", basename(urls))
#   urls <- setNames(urls, nms)
#   repo <- gsub("^(.+)/(.+)$", "\\2", repo)
#   eval(call("<-", as.symbol(repo), new.env()))
#
# # assign(repo, value = out)


# out <-   unclass(.parse_to_list(urls[[1]]))

# url <- urls[[1]]

#
# url = url,
# parsed_name = nm,
# obj_n = length(out),
# obj_names = names(out),
# obj_types = vapply(unname(out), mode, "")
#
#
# usethis:::standalone_choose
#
#
#


fn_from_call <- function (x, name = "function") {
  if (is_call(x, name)) {
    as.function(c(x[[2]], x[[3]]), envir = globalenv())
  } else {
    x
  }
}

assignment_from_call <- function(x, name = "<-") {
  if(is.list(x)) {
    out <- lapply(x, assignment_from_call)
    return(unlist(out))
  }
  if(is_call(x, name)) {
    nm <- deparse(x[[2]])
    value <- fn_from_call(x[[3]])
    setNames(list(value), nm)
  } else {
    x
  }
}

parse_to_list <- function (urls) {
  unlist(lapply(urls, .parse_to_list))
}

#
#
# .parse_to_list <- function(url, nm = NULL) {
#   nm <- nm %||% gsub(".[Rr]$", "", basename(url))
#   x <- as.list(parse(urls[[1]], keep.source = FALSE))
#   out <- assignment_from_call(x)
#   structure(.Data = out,
#             url = url,
#             parsed_name = nm,
#             obj_n = length(out),
#             obj_names = names(out),
#             obj_types = vapply(unname(out), mode, ""),
#             class = "parsed")
# }


# format.parsed <- function(x) {
#   info <- attributes(x)
#   out <- cbind(
#     file_name = rep(info$parsed_name, info$obj_n),
#     names = info$obj_names,
#     types = info$obj_types
#   )
#   row.names(out) <- rep("", info$obj_n)
#   out
# }



# print.parsed <- function (x) {
#
#   cat("<parsed>   ",
#       get_parsed_url(x),
#       "\n",
#       sep = "\n")
#   print(format(x))
#   invisible()
# }

is_parsed <- function (x) {
  inherits(x, "parsed")
}
get_parsed_name <- function (x) {
  stopifnot(is_parsed(x))
  attr(x, "parsed_name")
}

get_parsed_url <- function (x) {
  stopifnot(is_parsed(x))
  attr(x, "url")
}

