repo_urls <- function(repo_spec = "tidyverse/purrr", folder = "R") {
  input <- gh::gh(sprintf("/repos/%s/contents/%s", repo_spec, folder))
  vapply(input, getElement, "", "download_url")
}

repo_ns_exports <- function(repo_spec = "tidyverse/purrr") {
  input <- gh::gh(sprintf("/repos/%s/contents/NAMESPACE", repo_spec))
  x <- grep("^export", readLines(input$download_url), value = TRUE)
  x <- gsub("^export\\((.+)\\).*$", "\\1", x)
  vapply(x, \(x) as.character(str2lang(x)), "", USE.NAMES = FALSE)
}

assn_names <- function(url) {
  if(length(url) > 1) {
    out <- lapply(unname(url), sys.function())
    out <- do.call(rbind, out)
    return(out)
  }
  src <- parse(url, keep.source = TRUE)
  is_assign <- vapply(as.list(src), inherits, logical(1), "<-")
  nms <- lapply(as.list(src)[is_assign], getElement, 2)
  if(length(nms) == 0) return(NULL)

  file <- gsub("[.].+$", "", basename(url))
  if(grepl("^reexport|^superseded|^deprec|-package$|^package-", file)) {
    return(NULL)
  }
  name <- vapply(nms, deparse, "")
  data.frame(file = file,
             name_pos = seq_along(name),
             name = name,
             row.names = NULL)
}

repo_assn_names <- function(..., repo_spec = NULL) {
  if(is.null(repo_spec)) {
    url <- as.character(list(...))
    assn_names(url)
  } else {
    out <- assn_names(repo_urls(repo_spec))
    exports <- repo_ns_exports(repo_spec)
    out$export = out$name %in% exports
    out
  }
}
repo_spec = "tidyverse/purrr"
repo_assn_names(repo_spec = repo_spec)
