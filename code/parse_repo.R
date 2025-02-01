parse_repo <- function(repo, folder = "R", regex = NULL) {
  input <- gh::gh(sprintf("/repos/%s/contents/%s", repo, folder))
  urls <- vapply(input, getElement, "", "download_url")

  parse_to_list(urls)
}



parse_to_list <- function(urls, ...,  .named = TRUE, .assign_only = TRUE) {
  out <- lapply(urls, .parse_to_list, .assign_only)
  if(!.named) {
    return(out)
  }
  names(out) <- path_file(urls, rm_ext = TRUE)
  out
}


.parse_to_list <- function(urls, ..., .assign_only = TRUE) {
  x <- parse(urls, keep.source = FALSE)
  call_to_assignment(as.list(x))
}


call_to_assignment <- function(x, .assign_only = TRUE) {
  if(length(x) > 1) {
    out <- lapply(x, call_to_assignment, .assign_only)
    return(unlist(out))
  }
  if(is_call(x, "<-")) {
    value <- call_to_fn(x[[3]])
    names(value) <-deparse(x[[2]])
    return(value)
  }
  if(.assign_only) {
    NULL
  } else {
    x
  }

}

call_to_fn <- function (x) {
  if (is_call(x, "function")) {
    make_function(x[[2]], x[[3]])
  } else {
    x
  }
}






