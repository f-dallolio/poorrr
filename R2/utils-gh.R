gh_url_R <- function(repo) {
  json <- gh::gh(sprintf("/repos/%s/contents/%s", repo, "R"))
  urls <- unlist(lapply(json, getElement, "download_url"))
  names(urls) <- gsub("[.].+$", "", basename(urls))
  urls
}

gh_url_test <- function(repo) {
  json <- gh::gh(sprintf("/repos/%s/contents/%s", repo, "tests/testthat"))
  urls <- unlist(lapply(json, getElement, "download_url"))
  names(urls) <- gsub("^test-|[.].+$", "", basename(urls))
  urls
}

detect_test_fn <- function(fn_name, test_call) {
  if (is.list(test_call) || is.expression(test_call)) {
    out <- lapply(test_call, sys.function(), fn_name = fn_name)
    return(setNames(list(unlist(out)), fn_name))
  }
  stopifnot(is_string(fn_name))
  nms <- all.names(test_call[[3]], unique = TRUE)
  if (!fn_name %in% nms) {
    return(NULL)
  }
  test_call
}